{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module ShoppingCart
  ( createAProduct
  , createAnEmptyCart
  , numberOfProducts
  , addProducts
  , totalPriceWithTaxes
  , Product(..)
  , TaxAmount(..)
  , TotalPrice(..)
  , TotalPriceWithTax(..)
  , CartPrice(..)
  , Offer(..)
  ) where

import Data.Decimal
import Data.HashMap
import Data.Hashable
import GHC.Generics

data Offer = BuyXGetYFree
  { x :: Int
  , y :: Int
  } deriving (Show, Generic, Eq, Ord)

instance Hashable Offer

instance Hashable Decimal where
  hashWithSalt s x = s + hash (decimalMantissa x)

data Product = Product
  { name :: String
  , price :: Decimal
  , offer :: Maybe Offer
  } deriving (Show, Generic, Eq, Ord)

instance Hashable Product

newtype TaxAmount = TaxAmount
  { getTaxAmount :: Decimal
  } deriving (Show)

newtype TotalPrice = TotalPrice
  { getTotalPrice :: Decimal
  } deriving (Show)

newtype TotalPriceWithTax = TotalPriceWithTax
  { getTotalPriceWithTax :: Decimal
  } deriving (Show)

data CartPrice = CartPrice
  { getTotPriceWTax :: TotalPriceWithTax
  , getTotPrice :: TotalPrice
  , getTax :: TaxAmount
  , getTotalDiscountPrice :: Decimal
  } deriving (Show)

createAProduct :: String -> Decimal -> Maybe Offer -> Product
createAProduct = Product

newtype Cart = Cart
  { products :: Map Product Int
  } deriving (Show)

createAnEmptyCart :: Cart
createAnEmptyCart = Cart empty

numberOfProducts :: Cart -> Int
numberOfProducts = sum . elems . products

updateProductQuantity :: Product -> Int -> Int -> Int
updateProductQuantity prd newQuantity oldQuantity = newQuantity + oldQuantity

addProducts :: Cart -> Product -> Int -> Cart
addProducts cart product quantity =
  Cart (insertWithKey updateProductQuantity product quantity (products cart))

applyOffersToProducts :: Cart -> Map Product (Int, Int)
applyOffersToProducts cart = mapWithKey calculateCorrectQuantities (products cart)

calculateCorrectQuantities :: Product -> Int -> (Int, Int)
calculateCorrectQuantities (Product _ _ Nothing) quantity = (quantity, 0)
calculateCorrectQuantities product@(Product _ rate (Just (BuyXGetYFree x y))) quantity =
  let offerQuantity = div quantity (x + y)
      quantityAfterOffer = quantity - offerQuantity
  in (quantityAfterOffer, offerQuantity)

separateIntoTwoCarts :: Map Product (Int, Int) -> (Cart, Cart)
separateIntoTwoCarts mapWithCombinedQuantities =
  let (quantityAfterOfferCart, offerQuantityCart) =
        foldWithKey addRelevantQuantities (empty, empty) mapWithCombinedQuantities
  in (Cart quantityAfterOfferCart, Cart offerQuantityCart)

addRelevantQuantities :: Product
                      -> (Int, Int)
                      -> (Map Product Int, Map Product Int)
                      -> (Map Product Int, Map Product Int)
addRelevantQuantities product (quantityAfterOffer, offerQuantity) (quantityAfterOfferMap, offerQuantityMap) =
  ( (insert product quantityAfterOffer quantityAfterOfferMap)
  , (insert product offerQuantity offerQuantityMap))

--  | off (prd product) == Nothing = True
--  | otherwise = let offer = case (off (prd product)) of
privateTotalPrice :: Cart -> TotalPrice
privateTotalPrice cart = TotalPrice $ foldWithKey accumulatePrice 0.0 (products cart)

accumulatePrice :: Product -> Int -> Decimal -> Decimal
accumulatePrice product quantity accumulator =
  accumulator + ((fromIntegral quantity) * (price product))

privateTaxAmount :: TotalPrice -> Decimal -> TaxAmount
privateTaxAmount (TotalPrice totPrc) tax = TaxAmount (roundTo 2 $ (totPrc * tax) / 100)

totalPriceWithTaxes :: Cart -> Decimal -> CartPrice
totalPriceWithTaxes cart tax =
  let (quantityAfterOfferCart, offerQuantityCart) =
        separateIntoTwoCarts (applyOffersToProducts cart)
      tp = privateTotalPrice quantityAfterOfferCart
      taxAmount = privateTaxAmount tp tax
      tpWithTax = TotalPriceWithTax $ roundTo 2 $ getTotalPrice tp + getTaxAmount taxAmount
      totalDiscountPrice = getTotalPrice (privateTotalPrice offerQuantityCart)
  in CartPrice tpWithTax tp taxAmount totalDiscountPrice

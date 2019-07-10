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

data Offer
  = BuyXGetYFree { x :: Int
                ,  y :: Int}
  | BuyXGetYPercentOff { x :: Int
                      ,  percent :: Decimal}
  deriving (Show, Generic, Eq, Ord)

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

applyOffersToProducts :: Cart -> Map Product (Decimal, Decimal)
applyOffersToProducts cart = mapWithKey calculateCorrectQuantities (products cart)

calculateCorrectQuantities :: Product -> Int -> (Decimal, Decimal)
calculateCorrectQuantities (Product _ cost Nothing) quantity = (fromIntegral quantity * cost, 0)
calculateCorrectQuantities product@(Product _ rate (Just (BuyXGetYFree x y))) quantity =
  let offerQuantity = div quantity (x + y)
      quantityAfterOffer = quantity - offerQuantity
  in (fromIntegral quantityAfterOffer * rate, fromIntegral offerQuantity * rate)
calculateCorrectQuantities product@(Product _ rate (Just (BuyXGetYPercentOff x y))) quantity =
  let offerQuantity = div quantity (x + 1)
      quantityAfterOffer = quantity - offerQuantity
      discountedPrice = fromIntegral offerQuantity * rate * (y / 100)
      remainingPriceToBePaid = fromIntegral offerQuantity * rate * ((100 - y) / 100)
      totalPriceAfterDiscounts = fromIntegral quantityAfterOffer * rate + remainingPriceToBePaid
  in (totalPriceAfterDiscounts, discountedPrice)

accumulatePrice :: Product -> Int -> Decimal -> Decimal
accumulatePrice product quantity accumulator = accumulator + (fromIntegral quantity * price product)

privateTaxAmount :: TotalPrice -> Decimal -> TaxAmount
privateTaxAmount (TotalPrice totPrc) tax = TaxAmount (roundTo 2 $ (totPrc * tax) / 100)

totalPriceWithTaxes :: Cart -> Decimal -> CartPrice
totalPriceWithTaxes cart tax =
  let (totalPriceAfterDiscounts, totalDiscountedPrice) =
        foldl (<>) (0.0, 0.0) (elems (applyOffersToProducts cart))
      taxAmount = privateTaxAmount (TotalPrice totalPriceAfterDiscounts) tax
      tpWithTax =
        TotalPriceWithTax $
        roundTo 2 $ getTotalPrice (TotalPrice totalPriceAfterDiscounts) + getTaxAmount taxAmount
  in CartPrice
       tpWithTax
       (TotalPrice totalPriceAfterDiscounts)
       taxAmount
       (roundTo 2 totalDiscountedPrice)

instance Semigroup Decimal where
  (<>) :: Decimal -> Decimal -> Decimal
  (<>) = (+)

instance Monoid Decimal where
  mempty :: Decimal
  mempty = 0.0

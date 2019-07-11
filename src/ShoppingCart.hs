{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module ShoppingCart
  ( createAProduct
  , createAnEmptyCart
  , name
  , price
  , numberOfProducts
  , addProducts
  , totalPriceWithTaxes
  , Product(..)
  , TaxAmount(..)
  , TotalPrice(..)
  , TotalPriceWithTax(..)
  , CartPrice(..)
  , Offer(..)
  , ApplyOffer(..)
  , applyOffer
  , Cart(..)
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

class ApplyOffer a where
  applyOffer :: Product a -> Int -> (Decimal, Decimal)

instance ApplyOffer Offer where
  applyOffer :: Product Offer -> Int -> (Decimal, Decimal)
  applyOffer (Product _ cost Nothing) quantity = (fromIntegral quantity * cost, 0)
  applyOffer product@(Product _ rate (Just (BuyXGetYFree x y))) quantity =
    let offerQuantity = div quantity (x + y)
        quantityAfterOffer = quantity - offerQuantity
    in (fromIntegral quantityAfterOffer * rate, fromIntegral offerQuantity * rate)
  applyOffer product@(Product _ rate (Just (BuyXGetYPercentOff x y))) quantity =
    let offerQuantity = div quantity (x + 1)
        quantityAfterOffer = quantity - offerQuantity
        discountedPrice = fromIntegral offerQuantity * rate * (y / 100)
        remainingPriceToBePaid = fromIntegral offerQuantity * rate * ((100 - y) / 100)
        totalPriceAfterDiscounts = fromIntegral quantityAfterOffer * rate + remainingPriceToBePaid
    in (totalPriceAfterDiscounts, discountedPrice)

data Product a where
        Product ::
            ApplyOffer a => String -> Decimal -> Maybe a -> Product a

deriving instance Show (Product Offer)

instance ApplyOffer a =>
         Eq (Product a) where
  (==) :: Product a -> Product a -> Bool
  (==) (Product name _ _) (Product another _ _) = (==) name another

instance ApplyOffer a =>
         Ord (Product a) where
  (<=) :: Product a -> Product a -> Bool
  (<=) (Product name _ _) (Product anotherName _ _) = (<=) name anotherName

instance ApplyOffer a =>
         Hashable (Product a) where
  hashWithSalt s (Product name _ _) = s + hash name

name
  :: ApplyOffer a
  => Product a -> String
name (Product n _ _) = n

price
  :: ApplyOffer a
  => Product a -> Decimal
price (Product _ p _) = p

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

createAProduct
  :: ApplyOffer a
  => String -> Decimal -> Maybe a -> Product a
createAProduct = Product

data Cart a where
        Cart :: ApplyOffer a => Map (Product a) Int -> Cart a

deriving instance Show (Cart Offer)

products
  :: ApplyOffer a
  => Cart a -> Map (Product a) Int
products (Cart mp) = mp

createAnEmptyCart
  :: ApplyOffer a
  => Cart a
createAnEmptyCart = Cart empty

numberOfProducts
  :: ApplyOffer a
  => Cart a -> Int
numberOfProducts = sum . elems . products

updateProductQuantity
  :: ApplyOffer a
  => Product a -> Int -> Int -> Int
updateProductQuantity prd newQuantity oldQuantity = newQuantity + oldQuantity

addProducts
  :: ApplyOffer a
  => Cart a -> Product a -> Int -> Cart a
addProducts cart product quantity =
  Cart (insertWithKey updateProductQuantity product quantity (products cart))

applyOffersToProducts
  :: ApplyOffer a
  => Cart a -> Map (Product a) (Decimal, Decimal)
applyOffersToProducts cart = mapWithKey applyOffer (products cart)

{-
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
-}
accumulatePrice
  :: ApplyOffer a
  => Product a -> Int -> Decimal -> Decimal
accumulatePrice product quantity accumulator = accumulator + (fromIntegral quantity * price product)

privateTaxAmount :: TotalPrice -> Decimal -> TaxAmount
privateTaxAmount (TotalPrice totPrc) tax = TaxAmount (roundTo 2 $ (totPrc * tax) / 100)

totalPriceWithTaxes
  :: ApplyOffer a
  => Cart a -> Decimal -> CartPrice
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

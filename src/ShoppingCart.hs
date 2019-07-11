{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module ShoppingCart
  ( createAnEmptyCart
  , numberOfProducts
  , addProducts
  , totalPriceWithTaxes
  , TaxAmount(..)
  , TotalPrice(..)
  , TotalPriceWithTax(..)
  , CartPrice(..)
  , Cart(..)
  ) where

import Data.Decimal
import Data.HashMap
import Data.Hashable
import GHC.Generics
import Product

newtype TaxAmount = TaxAmount
  { getTaxAmount :: Decimal
  }

newtype TotalPrice = TotalPrice
  { getTotalPrice :: Decimal
  }

newtype TotalPriceWithTax = TotalPriceWithTax
  { getTotalPriceWithTax :: Decimal
  }

data CartPrice = CartPrice
  { getTotPriceWTax :: TotalPriceWithTax
  , getTotPrice :: TotalPrice
  , getTax :: TaxAmount
  , getTotalDiscountPrice :: Decimal
  }

data Cart a where
        Cart :: ApplyOffer a => Map (Product a) Int -> Cart a

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

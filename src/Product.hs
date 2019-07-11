{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Product
  ( name
  , price
  , ApplyOffer(..)
  , Offer(..)
  , createAProduct
  , Product(..)
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
  deriving (Generic, Eq, Ord)

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

createAProduct
  :: ApplyOffer a
  => String -> Decimal -> Maybe a -> Product a
createAProduct = Product

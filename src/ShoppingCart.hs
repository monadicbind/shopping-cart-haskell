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
  , Cart
  ) where

import Data.Decimal

data Product = Product
  { name :: String
  , price :: Decimal
  } deriving (Show)

data TaxAmount = TaxAmount
  { getTaxAmount :: Decimal
  } deriving (Show)

data TotalPrice = TotalPrice
  { getTotalPrice :: Decimal
  } deriving (Show)

data TotalPriceWithTax = TotalPriceWithTax
  { getTotPriceWTax :: Decimal
  , getTotPrice :: TotalPrice
  , getTax :: TaxAmount
  } deriving (Show)

createAProduct :: String -> Decimal -> Product
createAProduct = Product

data Cart = Cart
  { products :: [Product]
  } deriving (Show)

createAnEmptyCart :: Cart
createAnEmptyCart = Cart []

numberOfProducts :: Cart -> Int
numberOfProducts = length . products

addProducts :: Cart -> Product -> Int -> Cart
addProducts cart product quantity = Cart ((products cart) ++ (replicate quantity product))

privateTotalPrice :: Cart -> TotalPrice
privateTotalPrice cart = TotalPrice $ price (foldl (<>) mempty (products cart))

privateTaxAmount :: TotalPrice -> Decimal -> TaxAmount
privateTaxAmount (TotalPrice totPrc) tax = TaxAmount (roundTo 2 $ (totPrc * tax) / 100)

totalPriceWithTaxes :: Cart -> Decimal -> TotalPriceWithTax
totalPriceWithTaxes cart tax =
  let tp = privateTotalPrice cart
      taxAmount = privateTaxAmount tp tax
      tpWithTax = roundTo 2 $ (getTotalPrice tp) + (getTaxAmount taxAmount)
  in TotalPriceWithTax tpWithTax tp taxAmount

instance Semigroup Product where
  (<>) :: Product -> Product -> Product
  (<>) prd1 prd2 = Product "" (price prd1 + price prd2)

instance Monoid Product where
  mempty :: Product
  mempty = Product "" 0.0

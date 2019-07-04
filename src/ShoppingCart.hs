{-# LANGUAGE InstanceSigs #-}

module ShoppingCart
  ( createDoveSoapProduct
  , createAxeDeo
  , createAnEmptyCart
  , numberOfProducts
  , addProducts
  , totalPriceWithTaxes
  , Product(..)
  , TaxAmount(..)
  , TotalPrice(..)
  , TotalPriceWithTax(..)
  , CartPrice(..)
  , PrdCore(..)
  , Prd(..)
  ) where

import Data.Decimal

data Offer = BuyXGetYFree
  { x :: Int
  , y :: Int
  } deriving (Show)

data PrdCore = PrdCore
  { rate :: Decimal
  , off :: Maybe Offer
  } deriving (Show)

data Prd
  = DoveSoap { prd :: PrdCore}
  | AxeDeo { prd :: PrdCore}
  | NilProduct { prd :: PrdCore}
  deriving (Show)

data Product = Product
  { name :: String
  , price :: Decimal
  , offer :: Maybe Offer
  } deriving (Show)

data TaxAmount = TaxAmount
  { getTaxAmount :: Decimal
  } deriving (Show)

data TotalPrice = TotalPrice
  { getTotalPrice :: Decimal
  } deriving (Show)

data TotalPriceWithTax = TotalPriceWithTax
  { getTotalPriceWithTax :: Decimal
  } deriving (Show)

data CartPrice = CartPrice
  { getTotPriceWTax :: TotalPriceWithTax
  , getTotPrice :: TotalPrice
  , getTax :: TaxAmount
  } deriving (Show)

createDoveSoapProduct :: Decimal -> Maybe Offer -> Prd
createDoveSoapProduct rate offer = DoveSoap (PrdCore rate offer)

createAxeDeo :: Decimal -> Maybe Offer -> Prd
createAxeDeo rate offer = AxeDeo (PrdCore rate offer)

data Cart = Cart
  { products :: [Prd]
  } deriving (Show)

createAnEmptyCart :: Cart
createAnEmptyCart = Cart []

numberOfProducts :: Cart -> Int
numberOfProducts = length . products

addProducts :: Cart -> Prd -> Int -> Cart
addProducts cart product quantity = Cart ((products cart) ++ (replicate quantity product))

privateTotalPrice :: Cart -> TotalPrice
privateTotalPrice cart = TotalPrice $ (rate . prd) (foldl (<>) mempty (products cart))

privateTaxAmount :: TotalPrice -> Decimal -> TaxAmount
privateTaxAmount (TotalPrice totPrc) tax = TaxAmount (roundTo 2 $ (totPrc * tax) / 100)

totalPriceWithTaxes :: Cart -> Decimal -> CartPrice
totalPriceWithTaxes cart tax =
  let tp = privateTotalPrice cart
      taxAmount = privateTaxAmount tp tax
      tpWithTax = TotalPriceWithTax $ roundTo 2 $ (getTotalPrice tp) + (getTaxAmount taxAmount)
  in CartPrice tpWithTax tp taxAmount

instance Semigroup Prd where
  (<>) :: Prd -> Prd -> Prd
  (<>) prd1 prd2 = NilProduct (PrdCore (rate (prd prd1) + rate (prd prd2)) Nothing)

instance Monoid Prd where
  mempty :: Prd
  mempty = NilProduct (PrdCore 0.0 Nothing)

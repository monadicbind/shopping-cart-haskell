{-# LANGUAGE InstanceSigs #-}

module ShoppingCart where

import Data.Decimal

data Product = Product
  { name :: String
  , price :: Decimal
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

totalPrice :: Cart -> Decimal
totalPrice cart = price (foldl (<>) mempty (products cart))

instance Semigroup Product where
  (<>) :: Product -> Product -> Product
  (<>) prd1 prd2 = Product "" (price prd1 + price prd2)

instance Monoid Product where
  mempty :: Product
  mempty = Product "" 0.0

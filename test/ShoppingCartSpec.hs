module ShoppingCartSpec
  ( spec
  ) where

import ShoppingCart
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "Shopping Cart" $ do
    describe "Create a new product" $ do
      it "returns a product called with name Dove Soap" $ do
        let doveProduct = createAProduct "DoveSoap" 39.99
        name doveProduct `shouldBe` "DoveSoap"
        price doveProduct `shouldBe` 39.99
    describe "Create a new empty cart" $ do
      it "returns an empty cart" $ do
        let emptyCart = createAnEmptyCart
        numberOfProducts emptyCart `shouldBe` 0
    describe "Adding 5 DoveSoaps to an emptycart" $ do
      it " return a cart with 5 dove soaps" $ do
        let emptyCart = createAnEmptyCart
        let doveProduct = createAProduct "DoveSoap" 39.99
        let quantity = 5
        let newCart = addProducts emptyCart doveProduct quantity
        numberOfProducts newCart `shouldBe` quantity
        totalPrice newCart `shouldBe` 199.95
    describe "Adding 5 DoveSoaps and add 3 more to an emptycart" $ do
      it " return a cart with 8 dove soaps" $ do
        let emptyCart = createAnEmptyCart
        let doveProduct = createAProduct "DoveSoap" 39.99
        let quantity = 5
        let newCart = addProducts emptyCart doveProduct quantity
        let finalCart = addProducts newCart doveProduct 3
        numberOfProducts finalCart `shouldBe` quantity + 3
        totalPrice finalCart `shouldBe` 319.92

module ShoppingCartSpec
  ( spec
  ) where

import ShoppingCart
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "Shopping Cart" $ do
    describe "Create a new product" $
      it "returns a product called with name Dove Soap" $ do
        let doveProduct = createDoveSoapProduct 39.99 Nothing
      --name doveProduct `shouldBe` "DoveSoap"
        rate (prd doveProduct) `shouldBe` 39.99
    describe "Create a new empty cart" $
      it "returns an empty cart" $ do
        let emptyCart = createAnEmptyCart
        numberOfProducts emptyCart `shouldBe` 0
        (getTotalPriceWithTax . getTotPriceWTax) (totalPriceWithTaxes emptyCart 0) `shouldBe` 0
    describe "Adding 5 DoveSoaps to an emptycart" $
      it " return a cart with 5 dove soaps" $ do
        let emptyCart = createAnEmptyCart
        let doveProduct = createDoveSoapProduct 39.99 Nothing
        let quantity = 5
        let newCart = addProducts emptyCart doveProduct quantity
        numberOfProducts newCart `shouldBe` quantity
        (getTotalPriceWithTax . getTotPriceWTax) (totalPriceWithTaxes newCart 0) `shouldBe` 199.95
    describe "Adding 5 DoveSoaps and add 3 more to an emptycart" $
      it " return a cart with 8 dove soaps" $ do
        let emptyCart = createAnEmptyCart
        let doveProduct = createDoveSoapProduct 39.99 Nothing
        let quantity = 5
        let newCart = addProducts emptyCart doveProduct quantity
        let finalCart = addProducts newCart doveProduct 3
        numberOfProducts finalCart `shouldBe` quantity + 3
        (getTotalPriceWithTax . getTotPriceWTax) (totalPriceWithTaxes finalCart 0) `shouldBe` 319.92
    describe "Adding 2 DoveSoaps and 2 Axe Deo more to an emptycart" $
      it " return a cart with 4 products" $ do
        let emptyCart = createAnEmptyCart
        let doveProduct = createDoveSoapProduct 39.99 Nothing
        let quantity = 2
        let newCart = addProducts emptyCart doveProduct quantity
        let axeDeo = createAxeDeo 99.99 Nothing
        let taxRate = 12.5
        let finalCart = addProducts newCart axeDeo quantity
        let testableAmounts = totalPriceWithTaxes finalCart taxRate
        numberOfProducts finalCart `shouldBe` quantity + quantity
        (getTotalPriceWithTax . getTotPriceWTax) testableAmounts `shouldBe` 314.96
        (getTaxAmount . getTax) testableAmounts `shouldBe` 35.00
    describe "Adding 3 DoveSoaps with Buy2Get1Free Offer to an emptycart" $
      it " return a cart with 3 products" $ do
        let emptyCart = createAnEmptyCart
        let buy2get1 = BuyXGetYFree 2 1
        let doveProduct = createDoveSoapProduct 39.99 (Just buy2get1)
        let quantity = 3
        let finalCart = addProducts emptyCart doveProduct quantity
        let taxRate = 12.5
        let testableAmounts = totalPriceWithTaxes finalCart taxRate
        numberOfProducts finalCart `shouldBe` quantity
        (getTotalPriceWithTax . getTotPriceWTax) testableAmounts `shouldBe` 89.98
        getTotalDiscountPrice testableAmounts `shouldBe` 39.99
        (getTaxAmount . getTax) testableAmounts `shouldBe` 10.00
    describe "Adding 5 DoveSoaps to the cart" $
      it " return a cart with 5 products" $ do
        let emptyCart = createAnEmptyCart
        let buy2get1 = BuyXGetYFree 2 1
        let doveProduct = createDoveSoapProduct 39.99 (Just buy2get1)
        let quantity = 5
        let finalCart = addProducts emptyCart doveProduct quantity
        let taxRate = 12.5
        let testableAmounts = totalPriceWithTaxes finalCart taxRate
        numberOfProducts finalCart `shouldBe` quantity
        (getTotalPriceWithTax . getTotPriceWTax) testableAmounts `shouldBe` 179.96
        getTotalDiscountPrice testableAmounts `shouldBe` 39.99
        (getTaxAmount . getTax) testableAmounts `shouldBe` 20.00
    describe "Adding 3 DoveSoaps and 2 AxeDeo to the cart" $
      it " return a cart with 5 products" $ do
        let emptyCart = createAnEmptyCart
        let buy2get1 = BuyXGetYFree 2 1
        let doveProduct = createDoveSoapProduct 39.99 (Just buy2get1)
        let quantityDS = 3
        let dsCart = addProducts emptyCart doveProduct quantityDS
        let taxRate = 12.5
        let axeDeo = createAxeDeo 89.99 Nothing
        let finalCart = addProducts dsCart axeDeo 2
        let testableAmounts = totalPriceWithTaxes finalCart taxRate
        numberOfProducts finalCart `shouldBe` 5
        (getTotalPriceWithTax . getTotPriceWTax) testableAmounts `shouldBe` 292.46
        getTotalDiscountPrice testableAmounts `shouldBe` 39.99
        (getTaxAmount . getTax) testableAmounts `shouldBe` 32.50

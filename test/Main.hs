import           Test.Hspec       as TT
import           Test.Tasty       as TT
import           Test.Tasty.Hspec as TT

main :: IO ()
main = do
    test <- testSpec "haskell-webapi" spec
    defaultMain test

spec :: Spec
spec = parallel $ do
    it "Sum of 1 and 1 is 2" $ do
        1 + 1 `shouldBe` 2

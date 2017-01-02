import           Test.Hspec

main :: IO ()
main = do
    hspec emptyTestCase

emptyTestCase :: Spec
emptyTestCase = describe "Party API" $
    it "test suite not yet implemented" $
        "foo" `shouldBe` "foo"

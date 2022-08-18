import Analyze
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "--== Benchmark tests ==--" $ do
    describe "unit tests" $ do
      it "☯ analyze" $ do
        results <- analyze "echo" ["Hello"]
        output results `shouldBe` "Hello"
        time results > 0.0 `shouldBe` True
        memory results > 0.0 `shouldBe` True

    describe "hello-world" $ do
      let filename = "benchmarks/hello-world/hello"
      it "☯ python" $ do
        results <- analyze "python" [filename ++ ".py"]
        output results `shouldBe` "Hello"
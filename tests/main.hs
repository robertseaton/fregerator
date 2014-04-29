import Prover
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
      [ testGroup "QuickCheck Prover"
          [ testProperty "NNFEquiv" propNNFEquiv]
      ]

propNNFEquiv :: Formula -> Bool
propNNFEquiv f = f `equivalent` (nnf f)

import Prover
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
      [ testGroup "QuickCheck Prover"
          [ testProperty "NNFEquiv" propNNFEquiv
          , testProperty "DNFEquiv" propDNFEquiv
          , testProperty "CNFEquiv" propCNFEquiv]
      ]

--nnfEquiv 

propNNFEquiv :: Formula -> Bool
propNNFEquiv f = f `equivalent` (nnf f)

propDNFEquiv :: Formula -> Bool
propDNFEquiv f = f `equivalent` (dnf f)

propCNFEquiv :: Formula -> Bool
propCNFEquiv f = f `equivalent` (cnf f)

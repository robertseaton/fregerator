{-# OPTIONS_GHC -Wall #-}

import Prover
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--theorem", p] -> putStrLn p
    ["--help"] -> usage
    _ -> usage

usage :: IO ()
usage = do
  self <- getProgName
  putStr . unlines $
        concat ["Usage: ", self, " [OPTION] <theorem>"] :
        "Options:" :
        " --help Print this message" :
        " --theorem Attempt to solve a theorem" :
        []

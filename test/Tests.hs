module Main where

import Constrained.Tests as Tests
import Constrained.GraphSpec as Graph
import Data.Maybe
import System.Environment
import Test.Hspec

main :: IO ()
main = do
  nightly <- isJust <$> lookupEnv "NIGHTLY"
  hspec $ do
    Tests.tests nightly
    Graph.tests nightly

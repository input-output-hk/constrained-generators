module Main where

import Constrained.GraphSpec as Graph
import Constrained.Tests as Tests
import Data.Maybe
import System.Environment
import Test.Hspec

main :: IO ()
main = do
  nightly <- isJust <$> lookupEnv "NIGHTLY"
  hspec $ parallel $ do
    Tests.tests nightly
    Graph.tests nightly

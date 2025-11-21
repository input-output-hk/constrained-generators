{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Bench where

import Constrained.API
import Constrained.Examples.Basic
import Constrained.Examples.Map
import Constrained.Examples.Set
import Constrained.Generation
import Control.DeepSeq
import Criterion
import Data.Map (Map)
import Data.Set (Set)
import Data.Tree

benchmarks :: Benchmark
benchmarks =
  bgroup
    "constrained"
    [ benchSpec 10 30 "TrueSpec@Map" (trueSpec :: Specification (Map Int Int))
    , benchSpec 10 30 "TrueSpec@[]" (trueSpec :: Specification [Int])
    , benchSpec 10 30 "TrueSpec@Set" (trueSpec :: Specification (Set Int))
    , benchSpec
        10
        30
        "TrueSpec@Tree"
        (giveHint (Nothing, 30) <> trueSpec :: Specification (Tree Int))
    , benchSpec 10 30 "roseTreeMaybe" roseTreeMaybe
    , benchSpec 10 30 "listSumPair" listSumPair
    , benchSpec 10 30 "maybeJustSetSpec" maybeJustSetSpec
    , benchSpec 10 40 "eitherKeys" eitherKeys
    , benchSimplifySpec "eitherKeys" eitherKeys
    , benchSpec 10 40 "chooseBackwards" chooseBackwards'
    ]

roseTreeMaybe :: Specification (Tree (Maybe (Int, Int)))
roseTreeMaybe = constrained $ \t ->
  [ forAll' t $ \mp ts ->
      forAll ts $ \t' ->
        onJust mp $ \p ->
          onJust (rootLabel_ t') $ \p' ->
            fst_ p' ==. snd_ p
  , forAll' t $ \mp _ -> isJust mp
  , genHint (Nothing, 10) t
  ]

listSumPair :: Specification [(Int, Int)]
listSumPair = constrained $ \xs ->
  [ assert $ foldMap_ fst_ xs ==. 100
  , forAll' xs $ \x y -> [20 <. x, x <. 30, y <. 100]
  ]

benchSimplifySpec :: HasSpec a => String -> Specification a -> Benchmark
benchSimplifySpec nm spec =
  bench ("simplify/" ++ nm) $
    nf (show . simplifySpec) spec

benchSpec :: (HasSpec a, NFData a) => Int -> Int -> String -> Specification a -> Benchmark
benchSpec seed size nm spec =
  bench (unlines [nm, show (genFromSpecWithSeed seed size spec)]) $
    nf (genFromSpecWithSeed seed size) spec

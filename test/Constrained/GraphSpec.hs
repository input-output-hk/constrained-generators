{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingVia #-}
module Constrained.GraphSpec where

import Data.Either
import Constrained.Graph
import Data.Set (Set)
import Data.Set qualified as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype Node = Node Int
  deriving (Ord, Eq)
  deriving Show via Int

instance Arbitrary Node where
  arbitrary = Node <$> choose (0, 20)
  shrink (Node n) = Node <$> shrink n

prop_arbitrary_reasonable_distribution :: Graph Node -> Property
prop_arbitrary_reasonable_distribution g =
  cover 60 (isRight $ topsort g) "has topsort" True

prop_no_dependencies_topsort :: Set Node -> Property
prop_no_dependencies_topsort = property . isRight . topsort . noDependencies

prop_subtract_topsort :: Graph Node -> Graph Node -> Property
prop_subtract_topsort g g' =
  isRight (topsort g) ==>
    isRight (topsort $ subtractGraph g g')

prop_subtract_union :: Graph Node -> Graph Node -> Property
prop_subtract_union g g0' =
  let g' = subtractGraph g g0'
  in subtractGraph g g' <> g' === g

prop_subtract_keeps_nodes :: Graph Node -> Graph Node -> Property
prop_subtract_keeps_nodes g g' = nodes (subtractGraph g g') === nodes g

prop_subtract_removes_edges :: Graph Node -> Graph Node -> Node -> Node -> Property
prop_subtract_removes_edges g g' x y =
  property $ not (dependsOn x y (subtractGraph (dependency x (Set.singleton y) <> g) $ dependency x (Set.singleton y) <> g'))

prop_union_commutes :: Graph Node -> Graph Node -> Property
prop_union_commutes g g' = g <> g' === g' <> g

prop_delete_topsort :: Graph Node -> Node -> Property
prop_delete_topsort g n =
  isRight (topsort g) ==>
    isRight (topsort $ deleteNode n g)

prop_op_topsort :: Graph Node -> Property
prop_op_topsort g =
  isRight (topsort g) === isRight (topsort $ opGraph g)

prop_trC_topsort :: Graph Node -> Property
prop_trC_topsort g =
  isRight (topsort g) === isRight (topsort $ transitiveClosure g)

prop_trC_opgraph_commute :: Graph Node -> Property
prop_trC_opgraph_commute g =
  transitiveClosure (opGraph g) === opGraph (transitiveClosure g)

prop_depends_grows :: Graph Node -> Graph Node -> Node -> Property
prop_depends_grows g g' n = property $ dependencies n g `Set.isSubsetOf` dependencies n (g <> g')

prop_transitive_dependencies :: Graph Node -> Node -> Property
prop_transitive_dependencies g n =
  transitiveDependencies n g === dependencies n (transitiveClosure g)

prop_topsort_all_nodes :: Graph Node -> Property
prop_topsort_all_nodes g =
  case topsort g of
    Left{} -> discard
    Right o -> Set.fromList o === nodes g

prop_topsort_sound :: Graph Node -> Property
prop_topsort_sound g =
  case topsort g of
    Left{} -> discard
    Right o -> property $ go o
  where
    go [] = True
    go (n : ns) = all (\n' -> not $ dependsOn n n' g) ns && go ns

prop_topsort_complete :: Graph Node -> Property
prop_topsort_complete g =
  isLeft (topsort g) === not (null $ findCycle g)

prop_find_cycle_sound :: Property
prop_find_cycle_sound =
  forAllShrink (mkGraph @Node <$> arbitrary) shrink $ \ g ->
    let c = findCycle g
    in counterexample (show c) $ all (\(x, y) -> dependsOn x y g) (zip c (drop 1 $ cycle c))

prop_find_cycle_loops :: Property
prop_find_cycle_loops =
  forAllShrink (mkGraph @Node <$> arbitrary) shrink $ \ g ->
    case findCycle g of
      [] -> property True
      c@(x:_) -> cover 40 True "found cycle" $ counterexample (show c) $ dependsOn (last c) x g

return []

tests :: Bool -> Spec
tests _nightly =
  describe "Graph tests" $ sequence_ [ prop n (checkCoverage $ withMaxSuccess 1000 p) | (n, p) <- $allProperties ]

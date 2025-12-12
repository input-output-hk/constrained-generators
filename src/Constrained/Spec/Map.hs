{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | `HasSpec` instance for `Map` and functions for working with `Map`s
module Constrained.Spec.Map (
  MapSpec (..),
  defaultMapSpec,
  MapW (..),
  lookup_,
  mapMember_,
  dom_,
  rng_,
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generation
import Constrained.Generic (Prod (..))
import Constrained.List
import Constrained.NumOrd (cardinality, geqSpec, leqSpec, nubOrd)
import Constrained.PrettyUtils
import Constrained.Spec.List
import Constrained.Spec.Set
import Constrained.Spec.SumProd
import Constrained.Syntax
import Constrained.TheKnot
import Control.Monad
import Data.Foldable
import Data.Kind
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Prettyprinter
import Test.QuickCheck hiding (Fun, Witness, forAll)

------------------------------------------------------------------------
-- HasSpec
------------------------------------------------------------------------

instance Ord a => Sized (Map.Map a b) where
  sizeOf = toInteger . Map.size
  liftSizeSpec sz cant = typeSpec $ defaultMapSpec {mapSpecSize = TypeSpec sz cant}
  liftMemberSpec xs = case NE.nonEmpty (nubOrd xs) of
    Nothing -> ErrorSpec (pure "In liftMemberSpec for the (Sized Map) instance, xs is the empty list")
    Just ys -> typeSpec $ defaultMapSpec {mapSpecSize = MemberSpec ys}
  sizeOfTypeSpec (MapSpec _ mustk mustv size _ _) =
    geqSpec (sizeOf mustk)
      <> geqSpec (sizeOf mustv)
      <> size

-- | Custom `TypeSpec` for `Map`
data MapSpec k v = MapSpec
  { mapSpecHint :: Maybe Integer
  , mapSpecMustKeys :: Set k
  , mapSpecMustValues :: [v]
  , mapSpecSize :: Specification Integer
  , mapSpecElem :: Specification (k, v)
  , mapSpecFold :: FoldSpec v
  }
  deriving (Generic)

-- | emptySpec without all the constraints
defaultMapSpec :: Ord k => MapSpec k v
defaultMapSpec = MapSpec Nothing mempty mempty TrueSpec TrueSpec NoFold

instance
  ( HasSpec (k, v)
  , HasSpec k
  , HasSpec v
  , HasSpec [v]
  ) =>
  Pretty (WithPrec (MapSpec k v))
  where
  pretty (WithPrec d s) =
    parensIf (d > 10) $
      "MapSpec"
        /> vsep
          [ "hint       =" <+> viaShow (mapSpecHint s)
          , "mustKeys   =" <+> viaShow (mapSpecMustKeys s)
          , "mustValues =" <+> viaShow (mapSpecMustValues s)
          , "size       =" <+> pretty (mapSpecSize s)
          , "elem       =" <+> pretty (mapSpecElem s)
          , "fold       =" <+> pretty (mapSpecFold s)
          ]

instance
  ( HasSpec (k, v)
  , HasSpec k
  , HasSpec v
  , HasSpec [v]
  ) =>
  Show (MapSpec k v)
  where
  showsPrec d = shows . prettyPrec d

instance Ord k => Forallable (Map k v) (k, v) where
  fromForAllSpec kvs = typeSpec $ defaultMapSpec {mapSpecElem = kvs}
  forAllToList = Map.toList

-- ============================================================
-- We will need to take projections on (Specification (a,b))

fstSpec :: forall k v. (HasSpec k, HasSpec v) => Specification (k, v) -> Specification k
fstSpec s = mapSpec ProdFstW (mapSpec ToGenericW s)

sndSpec :: forall k v. (HasSpec k, HasSpec v) => Specification (k, v) -> Specification v
sndSpec s = mapSpec ProdSndW (mapSpec ToGenericW s)

-- ======================================================================
-- The HasSpec instance for Maps

instance
  (Ord k, HasSpec (Prod k v), HasSpec k, HasSpec v, HasSpec [v], IsNormalType k, IsNormalType v) =>
  HasSpec (Map k v)
  where
  type TypeSpec (Map k v) = MapSpec k v
  type Prerequisites (Map k v) = (HasSpec k, HasSpec v)

  emptySpec = defaultMapSpec

  combineSpec
    (MapSpec mHint mustKeys mustVals size kvs foldSpec)
    (MapSpec mHint' mustKeys' mustVals' size' kvs' foldSpec') = case combineFoldSpec foldSpec foldSpec' of
      Left msgs ->
        ErrorSpec $
          NE.fromList $
            [ "Error in combining FoldSpec in combineSpec for Map"
            , "  " ++ show foldSpec
            , "  " ++ show foldSpec'
            ]
              ++ msgs
      Right foldSpec'' ->
        typeSpec $
          MapSpec
            -- This is min because that allows more compositionality - if a spec specifies a
            -- low upper bound because some part of the spec will be slow it doesn't make sense
            -- to increase it somewhere else because that part isn't slow.
            (unionWithMaybe min mHint mHint')
            (mustKeys <> mustKeys')
            (nub $ mustVals <> mustVals')
            (size <> size')
            (kvs <> kvs')
            foldSpec''

  conformsTo m (MapSpec _ mustKeys mustVals size kvs foldSpec) =
    and
      [ mustKeys `Set.isSubsetOf` Map.keysSet m
      , all (`elem` Map.elems m) mustVals
      , sizeOf m `conformsToSpec` size
      , all (`conformsToSpec` kvs) (Map.toList m)
      , Map.elems m `conformsToFoldSpec` foldSpec
      ]

  genFromTypeSpec (MapSpec mHint mustKeys mustVals size (simplifySpec -> kvs) NoFold)
    | null mustKeys
    , null mustVals = do
        let size' =
              fold
                [ maybe TrueSpec (leqSpec . max 0) mHint
                , size
                , maxSpec (cardinality (fstSpec kvs))
                , maxSpec (cardinalTrueSpec @k)
                , geqSpec 0
                ]
        n <- genFromSpecT size'
        let go fc sz 0 slow kvs' m
              | fromInteger sz == Map.size m = pure m
              | not slow =
                  go
                    fc
                    sz
                    (sz - fromIntegral (Map.size m))
                    True
                    (kvs' <> typeSpec (Cartesian (notMemberSpec (Map.keys m)) mempty))
                    m
              | otherwise = fatalError "The impossible happened"
            go fc sz n' slow kvs' m = do
              mkv <- inspect $ genFromSpecT kvs'
              case mkv of
                Result (k, v) ->
                  go
                    fc
                    sz
                    (n' - 1)
                    slow
                    (kvs' <> if slow then typeSpec (Cartesian (notEqualSpec k) mempty) else mempty)
                    (Map.insert k v m)
                GenError {} | fc > 0 -> go (fc - 1) sz n' slow kvs' m
                GenError msgs ->
                  if sizeOf m `conformsToSpec` size
                    then pure m
                    else
                      genErrorNE
                        (pure "Gen error while trying to generate enough elements for a Map." <> catMessageList msgs)
                FatalError msgs ->
                  genErrorNE
                    ( NE.fromList
                        [ "Fatal error while trying to generate enough elements for a map:"
                        , "  The ones we have generated so far = " ++ show m
                        , "  The number we need to still generate: n' = " ++ show n'
                        , "The original size spec " ++ show size
                        , "The refined  size spec " ++ show size'
                        , "The computed target size " ++ show n
                        , "Fatal error messages"
                        , "<<<---"
                        ]
                        <> catMessageList msgs
                        <> (pure "--->>>")
                    )
        explain ("  The number we are trying for: n = " ++ show n) $ go (10 * n) n n False kvs mempty
  genFromTypeSpec (MapSpec mHint mustKeys mustVals size (simplifySpec -> kvs) foldSpec) = do
    !mustMap <- explain "Make the mustMap" $ forM (Set.toList mustKeys) $ \k -> do
      let vSpec = constrained $ \v -> satisfies (pair_ (Lit k) v) kvs
      v <- explain (show $ "vSpec =" <+> pretty vSpec) $ genFromSpecT vSpec
      pure (k, v)
    let haveVals = map snd mustMap
        mustVals' = filter (`notElem` haveVals) mustVals
        size' = simplifySpec $ constrained $ \sz ->
          -- TODO, we should make sure size' is greater than or equal to 0
          satisfies
            (sz + Lit (sizeOf mustMap))
            ( maybe TrueSpec (leqSpec . max 0) mHint
                <> size
                <> maxSpec (cardinality (fstSpec kvs)) -- (mapSpec FstW $ mapSpec ToGenericW kvs))
                <> maxSpec (cardinalTrueSpec @k)
            )
        !foldSpec' = case foldSpec of
          NoFold -> NoFold
          FoldSpec fn@(Fun symbol) sumSpec -> FoldSpec fn $ propagate theAddFn (HOLE :? Value mustSum :> Nil) sumSpec
            where
              mustSum = adds (map (semantics symbol) haveVals)
    let !valsSpec =
          ListSpec
            Nothing
            mustVals'
            size'
            (simplifySpec $ constrained $ \v -> unsafeExists $ \k -> pair_ k v `satisfies` kvs)
            foldSpec'

    !restVals <-
      explainNE
        ( NE.fromList
            [ "Make the restVals"
            , show $ "  valsSpec =" <+> pretty valsSpec
            , show $ "  mustMap =" <+> viaShow mustMap
            , show $ "  size' =" <+> pretty size'
            ]
        )
        $ genFromTypeSpec
        $ valsSpec
    let go m [] = pure m
        go m (v : restVals') = do
          let keySpec = notMemberSpec (Map.keysSet m) <> constrained (\k -> pair_ k (Lit v) `satisfies` kvs)
          k <-
            explainNE
              ( NE.fromList
                  [ "Make a key"
                  , show $ indent 4 $ "keySpec =" <+> pretty keySpec
                  ]
              )
              $ genFromSpecT keySpec
          go (Map.insert k v m) restVals'

    go (Map.fromList mustMap) restVals

  cardinalTypeSpec _ = TrueSpec

  shrinkWithTypeSpec (MapSpec _ _ _ _ kvs _) m = map Map.fromList $ shrinkList (shrinkWithSpec kvs) (Map.toList m)

  fixupWithTypeSpec _ _ = Nothing

  toPreds m (MapSpec mHint mustKeys mustVals size kvs foldSpec) =
    toPred
      [ Assert $ Lit mustKeys `subset_` dom_ m
      , forAll (Lit mustVals) $ \val ->
          val `elem_` rng_ m
      , sizeOf_ (rng_ m) `satisfies` size
      , forAll m $ \kv -> satisfies kv kvs
      , toPredsFoldSpec (rng_ m) foldSpec
      , maybe TruePred (`genHint` m) mHint
      ]

instance
  (Ord k, HasSpec k, HasSpec v, HasSpec [v], IsNormalType k, IsNormalType v) =>
  HasGenHint (Map k v)
  where
  type Hint (Map k v) = Integer
  giveHint h = typeSpec $ defaultMapSpec {mapSpecHint = Just h}

------------------------------------------------------------------------
-- Logic instances for
------------------------------------------------------------------------

-- | Function symbols for talking about maps
data MapW (dom :: [Type]) (rng :: Type) where
  DomW :: (HasSpec k, HasSpec v, IsNormalType k, IsNormalType v, Ord k) => MapW '[Map k v] (Set k)
  RngW :: (HasSpec k, HasSpec v, IsNormalType k, IsNormalType v, Ord k) => MapW '[Map k v] [v]
  LookupW ::
    (HasSpec k, HasSpec v, IsNormalType k, IsNormalType v, Ord k) => MapW '[k, Map k v] (Maybe v)

deriving instance Eq (MapW dom rng)

instance Semantics MapW where
  semantics DomW = Map.keysSet
  semantics RngW = Map.elems
  semantics LookupW = Map.lookup

instance Syntax MapW

instance Show (MapW d r) where
  show DomW = "dom_"
  show RngW = "rng_"
  show LookupW = "lookup_"

instance Logic MapW where
  propagate f ctxt (ExplainSpec es s) = explainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate DomW (Unary HOLE) spec =
    case spec of
      MemberSpec (s :| []) ->
        typeSpec $
          MapSpec Nothing s [] (equalSpec $ sizeOf s) TrueSpec NoFold
      TypeSpec (SetSpec must elemspec size) [] ->
        typeSpec $
          MapSpec
            Nothing
            must
            []
            size
            (constrained $ \kv -> satisfies (fst_ kv) elemspec)
            NoFold
      _ -> ErrorSpec (NE.fromList ["Dom on bad map spec", show spec])
  propagate RngW (Unary HOLE) spec =
    case spec of
      TypeSpec (ListSpec listHint must size elemspec foldspec) [] ->
        typeSpec $
          MapSpec
            listHint
            Set.empty
            must
            size
            (constrained $ \kv -> satisfies (snd_ kv) elemspec)
            foldspec
      -- NOTE: you'd think `MemberSpec [r]` was a safe and easy case. However, that
      -- requires not only that the elements of the map are fixed to what is in `r`,
      -- but they appear in the order that they are in `r`. That's
      -- very difficult to achieve!
      _ -> ErrorSpec (NE.fromList ["Rng on bad map spec", show spec])
  propagate LookupW (Value k :! Unary HOLE) spec =
    constrained $ \m ->
      [Assert $ Lit k `member_` dom_ m | not $ Nothing `conformsToSpec` spec]
        ++ [ forAll m $ \kv ->
               letBind (fst_ kv) $ \k' ->
                 letBind (snd_ kv) $ \v ->
                   whenTrue (Lit k ==. k') $
                     -- TODO: What you want to write is `just_ v `satisfies` spec` but we can't
                     -- do that because we don't have access to `IsNormalType v` here. When
                     -- we refactor the `IsNormalType` machinery we will be able to make
                     -- this nicer.
                     case spec of
                       MemberSpec as -> Assert $ v `elem_` Lit [a | Just a <- NE.toList as]
                       TypeSpec (SumSpec _ _ vspec) cant ->
                         v `satisfies` (vspec <> notMemberSpec [a | Just a <- cant])
           ]
  propagate LookupW (HOLE :? Value m :> Nil) spec =
    if Nothing `conformsToSpec` spec
      then notMemberSpec [k | (k, v) <- Map.toList m, not $ Just v `conformsToSpec` spec]
      else
        memberSpec
          (Map.keys $ Map.filter ((`conformsToSpec` spec) . Just) m)
          ( NE.fromList
              [ "propagate (lookup HOLE ms) on (MemberSpec ms)"
              , "forall pairs (d,r) in ms, no 'd' conforms to spec"
              , "  " ++ show spec
              ]
          )

  mapTypeSpec DomW (MapSpec _ mustSet _ sz kvSpec _) = typeSpec $ SetSpec mustSet (fstSpec kvSpec) sz
  mapTypeSpec RngW (MapSpec _ _ mustList sz kvSpec foldSpec) = typeSpec $ ListSpec Nothing mustList sz (sndSpec kvSpec) foldSpec

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

-- | Take the domain of a `Map` as a `Set`
dom_ ::
  (HasSpec (Map k v), HasSpec v, HasSpec k, Ord k, IsNormalType k, IsNormalType v) =>
  Term (Map k v) ->
  Term (Set k)
dom_ = appTerm DomW

-- | Take the range of a `Map` as a list
rng_ ::
  (HasSpec k, HasSpec v, Ord k, IsNormalType k, IsNormalType v) =>
  Term (Map k v) ->
  Term [v]
rng_ = appTerm RngW

-- | Lookup a key in the `Map`
lookup_ ::
  (HasSpec k, HasSpec v, Ord k, IsNormalType k, IsNormalType v) =>
  Term k ->
  Term (Map k v) ->
  Term (Maybe v)
lookup_ = appTerm LookupW

-- | Check if a key is a member of the map
mapMember_ ::
  (HasSpec k, HasSpec v, Ord k, IsNormalType k, IsNormalType v) =>
  Term k ->
  Term (Map k v) ->
  Term Bool
mapMember_ k m = not_ $ lookup_ k m ==. lit Nothing

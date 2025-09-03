{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Environments that map types variables to values
module Constrained.Env (
  Env,
  singleton,
  extend,
  lookup,
  find,
  remove,
  filterKeys,
) where

import Constrained.Core
import Constrained.GenT
import Constrained.PrettyUtils
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable
import Prettyprinter
import Prelude hiding (lookup)

-- | Typed environments for mapping @t`Var` a@ to @a@
newtype Env = Env {unEnv :: Map EnvKey EnvValue}
  deriving newtype (Semigroup, Monoid)
  deriving stock (Show)

data EnvValue where
  EnvValue :: (Typeable a, Show a) => !a -> EnvValue

deriving instance Show EnvValue

data EnvKey where
  EnvKey :: Typeable a => !(Var a) -> EnvKey

instance Eq EnvKey where
  EnvKey v == EnvKey v' = nameOf v == nameOf v'

instance Ord EnvKey where
  compare (EnvKey v) (EnvKey v') = compare (nameOf v) (nameOf v')

instance Show EnvKey where
  show (EnvKey var) = show var

-- | Extend an environment with a new variable value pair
extend :: (Typeable a, Show a) => Var a -> a -> Env -> Env
extend v a (Env m) = Env $ Map.insert (EnvKey v) (EnvValue a) m

-- | Remove a variable from an environment if it exists
remove :: Typeable a => Var a -> Env -> Env
remove v (Env m) = Env $ Map.delete (EnvKey v) m

-- | Create a singleton environment
singleton :: (Typeable a, Show a) => Var a -> a -> Env
singleton v a = Env $ Map.singleton (EnvKey v) (EnvValue a)

-- | Lookup a avariable in the environment
lookup :: Typeable a => Env -> Var a -> Maybe a
lookup (Env m) v = do
  EnvValue val <- Map.lookup (EnvKey v) m
  cast val

-- | `lookup` generalized to any `MonadGenError` monad @m@
find :: (Typeable a, MonadGenError m) => Env -> Var a -> m a
find env var = do
  case lookup env var of
    Just a -> pure a
    Nothing -> genError ("Couldn't find " ++ show var ++ " in " ++ show env)

-- | Filter the keys in an env, useful for removing irrelevant variables in
-- error messages
filterKeys :: Env -> (forall a. Typeable a => Var a -> Bool) -> Env
filterKeys (Env m) f = Env $ Map.filterWithKey (\ (EnvKey k) _ -> f k) m

instance Pretty EnvValue where
  pretty (EnvValue x) = pretty $ take 80 (show x)

instance Pretty EnvKey where
  pretty (EnvKey x) = viaShow x

instance Pretty Env where
  pretty (Env m) = "Env" /> vsep (map f (Map.toList m))
    where
      f (k, v) = hsep [pretty k, "->", pretty v]

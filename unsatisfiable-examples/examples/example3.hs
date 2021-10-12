{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -O0 -dno-typeable-binds #-}
{-# OPTIONS -fdefer-type-errors #-}
-- {-# OPTIONS_GHC -ddump-simpl #-}
-- {-# OPTIONS_GHC
--     -fprint-explicit-foralls
--     -fprint-explicit-coercions
--     -fprint-explicit-kinds
--     -fprint-equality-relations
--     -fprint-explicit-runtime-reps
--   #-}
{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin=Unsatisfiable #-}
-- | Note the @-fdefer-type-errors@.
--
-- This example compiles, but when run it prints:
--
-- @
-- example3: Unsatisfiable at  examples/example3.hs:34:13-38
-- This Max is not a Monoid
-- (deferred unsatisfiable type error)
-- @
--
module Main (main) where

import GHC.TypeLits
import Unsatisfiable

newtype Max a = Max a
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Max a) where
    Max a <> Max b = Max (max a b)

type Msg = 'Text "This Max is not a Monoid"
instance Unsatisfiable Msg => Monoid (Max a) where
    mempty = unsatisfiable -- @Msg is not necessary

main :: IO ()
main = do
    print $ foldMap Max [1,2,3 :: Int]

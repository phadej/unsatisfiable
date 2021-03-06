{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -O0 -dno-typeable-binds #-}
-- {-# OPTIONS -fdefer-type-errors #-}
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
-- | This example is showing that we don't need to add a @Ord a@
-- to the @Monoid (Max a)@ instance definition.
-- The plugin makes @'Unsatisfiable' Msg@ imply it.
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
    -- mempty = unsatisfiable @Msg
    -- the @Msg is not even required
    mempty = unsatisfiable

main :: IO ()
main = do
    print ()

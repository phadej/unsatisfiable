{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -dcore-lint -fplugin=Unsatisfiable #-}
-- | This is an example how to use 'unsatisfiable'
-- to conjure non-'Data.Kind.Type' expressions.
--
-- Alternatively the 'unsatisfiable' could be runtime representation
-- polymorphic, but this way we keep the library slightly simpler.
--
module Main (main, int) where

import Data.Void     (Void)
import GHC.Exts
import GHC.TypeLits
import Unsatisfiable

type Msg = 'Text "No."

int :: Unsatisfiable Msg => Int
int = I# (case unsatisfiable @Msg @Void of {})

main :: IO ()
main = print ()

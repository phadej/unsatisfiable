{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -dcore-lint -fplugin=Unsatisfiable #-}
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

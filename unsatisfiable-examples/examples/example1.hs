{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -dcore-lint -fplugin=Unsatisfiable #-}
-- | In this example we cannot use 'bar'
-- as @'Unsatisfiable' Msg@ is indeed unsatisfiable as the name says.
-- And GHC knows that.
--
-- The plugin however also prints the very helpful @No.@ message
-- we provided. In real world use that might make a difference.
--
module Main (main) where

import GHC.TypeLits
import Unsatisfiable

class Bar a where
    bar :: a -> a

type Msg = 'Text "No."
instance Unsatisfiable Msg => Bar Int where
    bar = unsatisfiable @Msg

main :: IO ()
main = print (bar (42 :: Int))

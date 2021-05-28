{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -dcore-lint -fplugin=Unsatisfiable #-}
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

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -dcore-lint -fplugin=Unsatisfiable #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -ddump-simpl #-}
module Main (main, demo) where

import GHC.TypeLits
import Unsatisfiable

type Msg a = 'Text "Something wrong with " ':<>: 'ShowType a

f :: Unsatisfiable (Msg a) => a -> a
f x = x+1

main :: IO ()
main = print (f True)

-- Let us create multiple dictionaries of Unsatisfiable (Msg Bool)
demo :: Bool
demo = f False

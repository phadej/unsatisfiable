{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -dcore-lint -fplugin=Unsatisfiable #-}
-- Unsatisfiable gives another way to define Bottom.
module Main (main, unsat) where

import Data.Constraint
import Unsatisfiable

-- Compare with 'Data.Constraint.bottom'.
unsat :: Unsatisfiable msg :- a
unsat = Sub unsatisfiable

main :: IO ()
main = print ()

{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -dcore-lint -fplugin=Unsatisfiable #-}
-- | The variant of
--
-- https://github.com/effectfully-ou/sketches/tree/master/custom-type-equality-errors
-- https://www.reddit.com/r/haskell/comments/nomdit/custom_type_equality_errors/
--
-- where we replace TypeError with Unsatisfiable.
--
-- The change is from
--
-- @
-- examples/example6.hs:51:7: error:
--     • "w" ':-> Bool is not equal to "w" ':-> Int
--     • In the first argument of ‘($)’, namely
--         ‘ext (Var :: (Var "w")) False’
--       In the second argument of ‘($)’, namely
--         ‘ext (Var :: (Var "w")) False $ Empty’
--       In the second argument of ‘($)’, namely
--         ‘ext (Var :: (Var "z")) True
--            $ ext (Var :: (Var "w")) False $ Empty’
--    |
-- 51 |     $ ext (Var :: (Var "w")) False
--    |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- @
--
-- to
--
-- @
-- examples/example6.hs:51:7: error:
--     Unsatisfiable:
--     "w" ':-> Bool is not equal to "w" ':-> Int
--    |
-- 51 |     $ ext (Var :: (Var "w")) False
--    |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- @
--
-- I.e. barely a change, but with more predicatable behaviour!
--
module Main where

import Data.Kind          (Constraint, Type)
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits
import Unsatisfiable

-------------------------------------------------------------------------------
-- bits from type-level-sets
-------------------------------------------------------------------------------

data Mapping k v = k :-> v

type Var :: Symbol -> Type
data Var k = Var

type Map :: [Mapping Symbol Type] -> Type
data Map n where
    Empty :: Map '[]
    Ext :: Var k -> v -> Map m -> Map ((k ':-> v) ': m)

-------------------------------------------------------------------------------
-- post EqualKV:
-------------------------------------------------------------------------------

-- | A helper type family which given the equality result
-- tells us the equality constraints or Unsatisfiable constraint.
type EqualKV :: k -> k -> v -> v -> Constraint
type family EqualKV k1 k2 v1 v2 where
    EqualKV k1 k1 v1 v1 = ()
    EqualKV k1 k2 v1 v2 =
        Unsatisfiable 
            ( 'ShowType (k1 ':-> v1) ':<>:
              'Text " is not equal to " ':<>:
              'ShowType (k2 ':-> v2)
            )

-- | Compare to
--
-- @
-- Prelude> data X = X
-- Prelude> show (max X X)
-- 
-- <interactive>:2:1: error:
--     • No instance for (Show X) arising from a use of ‘show’
--     • In the expression: show (max X X)
--       In an equation for ‘it’: it = show (max X X)
-- 
-- <interactive>:2:7: error:
--     • No instance for (Ord X) arising from a use of ‘max’
--     • In the first argument of ‘show’, namely ‘(max X X)’
--       In the expression: show (max X X)
--       In an equation for ‘it’: it = show (max X X)
-- @
--
-- Here, GHC simply reports both unsatisfiable constraints as well.
--
-- (I don't use separate class-alias type-class, as it's not really needed).
-- 
ext :: (EqualKV k1 k2 v1 v2, k1 ~ k2, v1 ~ v2) => Var k1 -> v1 -> Map m -> Map ((k2 ':-> v2) ': m)
ext = Ext

foo :: Map '["x" :-> Int, "z" :-> Bool, "w" :-> Int]
foo = ext (Var @"x") 2
    $ ext (Var @"z") True
    $ ext (Var @"w") False
    $ Empty

-------------------------------------------------------------------------------
-- to make this an executable
-------------------------------------------------------------------------------

main :: IO ()
main = return ()

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
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
import Data.Foldable

data Pair a b = Pair a b
  deriving (Functor)

instance Warning
    ('Text "Some don't like Foldable instance for Pairs: "
    ':<>: 'ShowType a)
    => Foldable (Pair a) where
    foldMap f (Pair _ x) = f x
    toList (Pair _ x) = [x]
    null _ = False
    length _ = 1

-- | This warns too.
instance Traversable (Pair a) where
    traverse f (Pair x y) = Pair x <$> f y

main :: IO ()
main = do
    -- warns
    print (length (Pair 'x' True))

    -- doesn't warn as this instance is already solved!
    print (null (Pair 'x' True))

{-
Should warn when compiling:

examples/exampleWarning1.hs:40:12: warning: [-Wdeprecations]
    Warning instance:
    Some don't like Foldable instance for Pairs: Char
   |
40 |     print (length (Pair 'x' True))
   |            ^^^^^^^^^^^^^^^^^^^^^^

examples/exampleWarning1.hs:34:10: warning: [-Wdeprecations]
    Warning instance:
    Some don't like Foldable instance for Pairs: a
   |
34 | instance Traversable (Pair a) where
   |

-}

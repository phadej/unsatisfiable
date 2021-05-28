{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | 'Unsatisfiable' type-class.
module Unsatisfiable.Class (
    Unsatisfiable,
    unsatisfiable,
) where

import GHC.TypeLits (ErrorMessage)

-- | 'Unsatisfiable' type-class.
--
-- This a common idiom, to have a type-class with a non-exported member.
-- This class cannot be ever instantiated, and we can use it
-- to "forbid" some instances from existence, by defining
-- an instance with unsatisfiable constraint.
--
-- The 'unsatisfiable' acts as better 'undefined', using this type-class
-- constraint.
--
-- The behaviour of this class would be rather better defined than
-- instantiating the polymorphic 'GHC.TypeLits.TypeError' to kind
-- 'Data.Kind.Constraint', because it is clear when to report the custom error:
-- instead of an unsolved constraint error, when the constraint solver fails to
-- solve a wanted @'Unsatisfiable' msg@.
--
-- The custom error reporting is done using 'Unsatisfiable.plugin' type-checker
-- plugin.
--
-- See discussion in [GHC#18310](https://gitlab.haskell.org/ghc/ghc/-/issues/18310).
--
-- There are some examples of using this class and plugin
-- at https://github.com/phadej/unsatisfiable/tree/master/unsatisfiable-examples/examples
--
class Unsatisfiable (msg :: ErrorMessage) where
    unsatisfiable_ :: a

-- | See 'Unsatisfiable'.
--
-- If you need some other @TYPE@, you can use @case@ trick by
-- instantiating 'unsatisfiable' to 'Data.Void.Void' and using @EmptyCase@.
--
-- @
-- case 'unsatisfiable' @msg @Void of {}
-- @
--
unsatisfiable :: forall msg a. Unsatisfiable msg => a
unsatisfiable = unsatisfiable_ @msg @a

{-# language DuplicateRecordFields #-}
{-# language ScopedTypeVariables #-}
{-# language FunctionalDependencies  #-}
{-# language NoMonomorphismRestriction #-}
{-# language OverloadedLabels #-}
{-# language DataKinds #-}
{-# language AllowAmbiguousTypes #-}

-- Only for warnings/examples
{-# language PartialTypeSignatures #-}

import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Proxy

-- * The Type Class

-- | OK, so this is a possible interface for updating a field. We accept
-- a `sym :: Symbol` signifying the record label, and an @s@ for the source
-- record, a @t@ for the target record, and finally an @a@ for the type of
-- the field.
--
-- Note that this only provides field set functionality, and not
-- modification. An alternative design might incorporate another type
-- parameter which would represent the value of the field from the input
-- record - a la,
--
-- > setField :: Proxy sym -> (a -> b) -> s -> t
--
-- I chose not to do this, because conceptually, "modify" is a composition
-- of "get" and "set".
class SetField (sym :: Symbol) s t a | sym s t -> a where
    setField :: a -> s -> t

-- | This is used to make the API a bit easier to use. I could have just
-- done type applications. I don't remember why I did this instead.
instance (KnownSymbol sym, sym ~ sym') => IsLabel sym (Proxy sym') where
    fromLabel = Proxy


-- * Datatypes and Instances

data A = A { name :: String, age :: Int }

-- In the case of monomorphic update, we can introduce a constraint that
-- should fix the target type. This equality constraint will help make type
-- inference better.
instance (A ~ t) => SetField "name" A t String where
    setField str (A _oldStr oldAge) = A str oldAge

instance (A ~ t) => SetField "age" A t Int where
    setField age (A oldStr _oldAge) = A oldStr age

-- | Here's a neat trick that this design allows. This type is like 'A',
-- but only has a name.
data OnlyName = OnlyName { name :: String }

-- | We get the obvious instance here.
instance (OnlyName ~ t) => SetField "name" OnlyName t String where
    setField str (OnlyName _oldStr) = OnlyName str


-- | And then we get this synthetic instance.
--
-- This instance should not be allowed... or should it?
--
-- We're allowing users to perform type changing updates, and that's
-- *usually* so that we can change the type of a type parameter. But if we
-- do allow folks to write instances that do more than that, is it
-- a problem? I don't think so.
--
-- This might allow folks to write really nice builder pattern API for
-- safely constructing values incrementally.
instance (A ~ t) => SetField "age" OnlyName t Int where
    setField age (OnlyName oldStr) = A oldStr age

createA :: Int -> OnlyName -> A
createA age onlyName = setField @"age" age onlyName
-- with sugar,
    -- onlyName { age = age }

data B = B { name :: Int, age :: String}

instance (B ~ t) => SetField "name" B t Int where
    setField int (B _oldInt oldStr) = B int oldStr

instance (B ~ t) => SetField "age" B t String where
    setField str (B oldInt _oldStr) = B oldInt str

-- I leave off type signatures here as a test to see how well GHC can do
-- type inference. These all infer fine.
defaultA =
    A { name = "goodbye", age = 10 }

singleUpdate =
    setField @"name" "hello" defaultA

multiUpdate =
    setField @"name" "hello" $ setField @"age" (5 :: Int) defaultA

singleUpdateFn =
    setField @"name" "hello"

-- | This function demonstrates a multi-update chain with the 'SetField'
-- class. By itself, the updates don't work - the intermediate type is
-- lost. However, if we add constraints that *force* the input and outputs
-- to line up, then GHC is happy with it.
--
-- This suggests that we can desugar a record update expression to a series
-- of 'setField' calls along with a few extra equality.
multiUpdateFn
    :: forall nameS nameT ageS ageT a.
    ( SetField "name" nameS nameT String
    , SetField "age" nameS nameS a
    , ageT ~ nameS
    -- ^ A constraint that the output of updating the age is the same as
    -- the input to updating the name
    , ageT ~ ageS
    -- ^ This forces the input and output of the age update to be the same
    , Num a
    )
    => ageS
    -> nameT
multiUpdateFn x =
    (setField @"name" @nameS @nameT "hello" :: nameS -> nameT)
        (setField @"age" @ageS @ageT 5 x :: nameS)

-- OK, so this is the real core of what's necessary for GHC to change in
-- order to make this work. We have an expression:
--
-- > x { name = "hello", age = 5 }
--
-- GHC can desugar this to:
--
-- > setField @"name" "hello" (setField @"age" 5 x)
--
-- However, without additional constraints, GHC is unhappy with this
-- - there's ambiguity.
--
-- > recupd.hs:119:1: error: [GHC-39999]
-- >     • Could not deduce ‘SetField "name" s0 t String’
-- >       from the context: SetField "name" s t String
-- >         bound by the inferred type for ‘multiUpdateFn’:
-- >                    forall {s} {t} {p}. SetField "name" s t String => p -> t
-- >         at recupd.hs:(119,1)-(120,60)
-- >       The type variable ‘s0’ is ambiguous
-- >       Potentially matching instances:
-- >         instance (A ~ t) => SetField "name" A t String
-- >           -- Defined at recupd.hs:47:10
-- >         instance (OnlyName ~ t) => SetField "name" OnlyName t String
-- >           -- Defined at recupd.hs:56:10
-- >         ...plus three others
-- >         (use -fprint-potential-instances to see them all)
-- >     • In the ambiguity check for the inferred type for ‘multiUpdateFn’
-- >       To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
-- >       When checking the inferred type
-- >         multiUpdateFn :: forall {s} {t} {p}.
-- >                          SetField "name" s t String =>
-- >                          p -> t
-- >     |
-- > 119 | multiUpdateFn x =
-- >     | ^^^^^^^^^^^^^^^^^...
--
-- So we need to emit a few extra constraints - namely, that the result of
-- updating the age field of `x` must be the source of updating the name
-- field of `x`. We also require that the source and target of updating the
-- age remains the same.

updateNameAgeA :: A -> A
updateNameAgeA a = a { name = "hello", age = 5 }

-- * Polymorphic Updates

-- $polyupdates
--
-- So it's one thing to get monomorphic updates. Can we get polymorphic
-- updates? Let's start with a polymorphic type.

data PolyA a = PolyA { name :: a, age :: Int }

instance  SetField "name" (PolyA a) (PolyA b) b where
    setField str (PolyA _oldStr oldAge) = PolyA str oldAge

instance (PolyA a ~ t) => SetField "age" (PolyA a) t Int where
    setField age (PolyA oldStr _oldAge) = PolyA oldStr age

defaultPolyA :: PolyA String
defaultPolyA = PolyA "asdf" 44

-- | Neat! our multi update works for a @'PolyA' 'String'@.
wow :: PolyA String
wow = multiUpdateFn defaultPolyA

replaceNameWithBool
    :: forall nameS nameT ageS ageT a.
    ( Num a
    , SetField "name" nameS nameT Bool
    , SetField "age" ageS ageT a
    , ageT ~ nameS
    -- ^ A constraint that the output of updating the age is the same as
    -- the input to updating the name
    , ageT ~ ageS
    -- ^ This forces the input and output of the age update to be the same
    )
    => ageS
    -> nameT
replaceNameWithBool x =
    setField @"name" True (setField @"age" @ageS @ageT @a (5 :: a) x :: ageT)

-- | This is just like 'replaceNameWithBool', but it
replaceNameWithBoolSwap
    :: forall nameS nameT ageS ageT.
    ( SetField "name" nameS nameT Bool
    , SetField "age" ageS ageT Int
    , ageS ~ nameT
    -- ^ A constraint that the output of updating the age is the same as
    -- the input to updating the name
    , ageS ~ ageT
    -- ^ This forces the input and output of the age update to be the same
    )
    => nameS
    -> ageT
replaceNameWithBoolSwap x =
    setField @"age" (5 :: Int) (setField @"name" True x :: nameT)

wow' :: PolyA Bool
wow' = replaceNameWithBool defaultPolyA

wow'' :: PolyA Bool
wow'' = replaceNameWithBoolSwap defaultPolyA

data TwoPoly a b = TwoPoly { name :: a, age :: b }

instance (b ~ d) => SetField "name" (TwoPoly a b) (TwoPoly c d) c where
    setField str (TwoPoly _oldStr oldAge) = TwoPoly str oldAge

instance (b ~ d) => SetField "age" (TwoPoly b a) (TwoPoly d c) c where
    setField age (TwoPoly oldStr _oldAge) = TwoPoly oldStr age

defaultTwoPoly :: TwoPoly String Int
defaultTwoPoly = TwoPoly "asdf" 33

wowTwoPoly = replaceNameWithBool defaultTwoPoly
wowTwoPoly' = replaceNameWithBoolSwap defaultTwoPoly

-- | A challenger approaches.
--
-- This type has the same type variable in two slots, so we can't change
-- the type in a field update unless we update *both* fields. This means we
-- can't simply splice in a composition of field updates when the type is
-- changing.
data SamePolyVar a = SamePolyVar { name :: a, age :: a }

instance (t ~ SamePolyVar a) => SetField "name" (SamePolyVar a) t a where
    setField str (SamePolyVar _oldStr oldAge) = SamePolyVar str oldAge

instance (t ~ SamePolyVar a) => SetField "age" (SamePolyVar a) t a where
    setField age (SamePolyVar oldStr _oldAge) = SamePolyVar oldStr age

-- | But type inference works fine, if the input type is known.
updateSamePolyVar :: SamePolyVar Int -> _ -- SamePolyVar Int
updateSamePolyVar =
    setField @"name" 10 . setField @"age" 20

-- * Solving Multiple Fields with Shared Type
--
-- OK, so, dang. If we have two fields with the same type variable, then we
-- can either:
--
-- 1. Do a non-type changing update with a single field, or
-- 2. Do a type-changing update with all fields
--
-- This is introducing a concept: "update *fields*" instead of the simpler
-- "update field".
--
-- This suggests a new mechanism may be necessary...

main = pure ()

{-# language DuplicateRecordFields #-}
{-# language ScopedTypeVariables #-}
{-# language FunctionalDependencies  #-}
{-# language NoMonomorphismRestriction #-}
{-# language OverloadedLabels #-}
{-# language DataKinds #-}

import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Proxy

-- * The Type Class

class UpdateField (sym :: Symbol) s t a | sym s t -> a where
    updateField :: Proxy sym -> a -> s -> t

-- * Datatypes and Instances

data A = A { name :: String, age :: Int }

-- In the case of monomorphic update, we can introduce a constraint that
-- should fix the target type.
instance (A ~ t) => UpdateField "name" A t String where
    updateField _ str (A _oldStr oldAge) = A str oldAge

instance (A ~ t) => UpdateField "age" A t Int where
    updateField _ age (A oldStr _oldAge) = A oldStr age

data A' = A' { name :: String, age :: Int }

-- This instance should not be allowed... or should it?
instance UpdateField "name" A A' String where
    updateField _ name (A _ i) = A' name i

instance (A' ~ t) => UpdateField "name" A' t String where
    updateField _ str (A' _oldStr oldA'ge) = A' str oldA'ge

instance (A' ~ t) => UpdateField "age" A' t Int where
    updateField _ age (A' oldStr _oldA'ge) = A' oldStr age

data B = B { name :: Int, age :: String}

instance (B ~ t) => UpdateField "name" B t Int where
    updateField _ int (B _oldInt oldStr) = B int oldStr

instance (B ~ t) => UpdateField "age" B t String where
    updateField _ str (B oldInt _oldStr) = B oldInt str

instance (KnownSymbol sym, sym ~ sym') => IsLabel sym (Proxy sym') where
    fromLabel = Proxy

defaultA = A { name = "goodbye", age = 10 }

singleUpdate = updateField #name "hello" (A { name = "goodbye", age = 10 })

multiUpdate =
    updateField #name "hello" $ updateField #age (5 :: Int) defaultA

singleUpdateFn =
    updateField #name "hello"

-- | This function demonstrates a multi-update chain with the 'UpdateField'
-- class. By itself, the updates don't work - the intermediate type is
-- lost. However, if we add constraints that *force* the input and outputs
-- to line up, then GHC is happy with it.
--
-- This suggests that we can desugar a record update expression to a series
-- of 'updateField' calls along with a few wanted constraints.
multiUpdateFn
    :: forall nameS nameT ageS ageT.
    ( UpdateField "name" nameS nameT String
    , UpdateField "age" ageS ageT Int
    , ageT ~ nameS
    -- ^ A constraint that the output of updating the age is the same as
    -- the input to updating the name
    , ageT ~ ageS
    -- ^ This forces the input and output of the age update to be the same
    )
    => ageS
    -> nameT
multiUpdateFn x =
    updateField #name "hello" (updateField #age (5 :: Int) x :: ageT)

updateNameAgeA :: A -> A
updateNameAgeA a = a { name = "hello", age = 5 }

-- * Polymorphic Updates

-- $polyupdates
--
-- So it's one thing to get monomorphic updates.

data PolyA a = PolyA { name :: a, age :: Int }

instance  UpdateField "name" (PolyA a) (PolyA b) b where
    updateField _ str (PolyA _oldStr oldAge) = PolyA str oldAge

instance (PolyA a ~ t) => UpdateField "age" (PolyA a) t Int where
    updateField _ age (PolyA oldStr _oldAge) = PolyA oldStr age

defaultPolyA :: PolyA String
defaultPolyA = PolyA "asdf" 44

-- | Neat! our multi update works for a @'PolyA' 'String'@.
wow :: PolyA String
wow = multiUpdateFn defaultPolyA

replaceNameWithBool
    :: forall nameS nameT ageS ageT a.
    ( Num a
    , UpdateField "name" nameS nameT Bool
    , UpdateField "age" ageS ageT a
    , ageT ~ nameS
    -- ^ A constraint that the output of updating the age is the same as
    -- the input to updating the name
    , ageT ~ ageS
    -- ^ This forces the input and output of the age update to be the same
    )
    => ageS
    -> nameT
replaceNameWithBool x =
    updateField #name True (updateField @"age" @ageS @ageT @a #age (5 :: a) x :: ageT)

-- | This is just like 'replaceNameWithBool', but it
replaceNameWithBoolSwap
    :: forall nameS nameT ageS ageT.
    ( UpdateField "name" nameS nameT Bool
    , UpdateField "age" ageS ageT Int
    , ageS ~ nameT
    -- ^ A constraint that the output of updating the age is the same as
    -- the input to updating the name
    , ageS ~ ageT
    -- ^ This forces the input and output of the age update to be the same
    )
    => nameS
    -> ageT
replaceNameWithBoolSwap x =
    updateField #age (5 :: Int) (updateField @"name" #name True x :: nameT)

wow' :: PolyA Bool
wow' = replaceNameWithBool defaultPolyA

wow'' :: PolyA Bool
wow'' = replaceNameWithBoolSwap defaultPolyA

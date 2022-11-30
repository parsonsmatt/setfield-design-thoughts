{-# language DuplicateRecordFields #-}
{-# language NoMonomorphismRestriction #-}
{-# language OverloadedLabels #-}
{-# language DataKinds #-}

import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Proxy

data A = A { name :: String, age :: Int }

data B = B { name :: Int, age :: String}

class UpdateField (sym :: Symbol) s t a where
    updateField :: Proxy sym -> a -> s -> t

-- In the case of monomorphic update, we can introduce a constraint that
-- should fix the target type.
instance (A ~ t) => UpdateField "name" A t String where
    updateField _ str (A _oldStr oldAge) = A str oldAge

instance (A ~ t) => UpdateField "age" A t Int where
    updateField _ age (A oldStr _oldAge) = A oldStr age

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

multiUpdateFn x =
    updateField #name "hello" $ updateField #age (5 :: Int) x

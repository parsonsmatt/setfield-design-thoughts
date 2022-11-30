{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}

import GHC.TypeLits
import Data.Proxy

data A = A { name :: String, age :: Int }

data B = B { name :: Int, age :: String}

class UpdateField (sym :: Symbol) s t a where
    updateField :: proxy sym -> a -> s -> t

instance UpdateField "name" A A String where
    updateField _ str (A _oldStr oldAge) = A str oldAge

instance UpdateField "age" A A Int where
    updateField _ age (A oldStr _oldAge) = A oldStr age

instance UpdateField "name" B B Int where
    updateField _ int (B _oldInt oldStr) = B int oldStr

instance UpdateField "age" B B String where
    updateField _ str (B oldInt _oldStr) = B oldInt str

foo = updateField (Proxy @"name") "hello" (A { name = "goodbye", age = 10 })

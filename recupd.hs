{-# language DuplicateRecordFields #-}

data A = A { name :: String }

data B = B { name :: Int }

class UpdateField

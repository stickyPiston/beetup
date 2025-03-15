module Domain.User where

import Data.UUID (UUID)

data User = User 
  { id :: UUID
  , username :: String
  , name :: String
  , password :: String
  }

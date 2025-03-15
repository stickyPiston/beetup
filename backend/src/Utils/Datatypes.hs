module Utils.Datatypes where

import Data.IORef (IORef)
import qualified Data.Map as M
import Data.UUID (UUID)

type Sessions = IORef (M.Map UUID Int)

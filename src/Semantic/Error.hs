module Semantic.Error where

import Data.ByteString (ByteString)

data TypeError
  = TypeMismatch
  | SubtypingLoop
  | ExprHasNoType
  | UndeclaredIdentifier ByteString
  deriving (Show)

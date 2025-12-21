module Semantic.Error where

import Data.ByteString (ByteString)

data TypeError
  = TypeMismatch
  | SubtypingLoop
  | ExprHasNoType
  | IsNotSubtypeOf ByteString ByteString
  | UndeclaredIdentifier ByteString
  deriving (Show)

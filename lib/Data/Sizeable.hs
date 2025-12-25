module Data.Sizeable where

import Data.ByteString (ByteString, length)

import Lexer.Lexer (Lexeme (..))

class Sizeable a where
  size :: a -> Int

  max :: a -> a -> a
  max lhs rhs = if size lhs >= size rhs then lhs else rhs

  min :: a -> a -> a
  min lhs rhs = if size lhs <= size rhs then lhs else rhs

instance Sizeable [a] where
  size = Prelude.length

instance Sizeable ByteString where
  size = Data.ByteString.length

instance Sizeable Lexeme where
  -- FIX: this doesn't work properly
  -- because of how tag implements show
  size (Lexeme tag Nothing) = size $ show tag
  size (Lexeme _ (Just s)) = size s

module Parser.Parser (
  MProgram (..),
  Typeid (..),
  Objectid (..),
  Program (..),
  Class (..),
  Feature (..),
  Formal (..),
  Binding (..),
  PatternMatch (..),
  AST (..),
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Parser.Position

data MProgram
  = MProgram
  { _entryPoint :: Program
  , _filename :: FilePath
  }

data Typeid = Typeid ByteString | NoType

newtype Objectid = Objectid ByteString

newtype Program = Program [Class]

data Class = Class Typeid [Feature] FilePath | ClassInherits Typeid Typeid [Feature] FilePath

data Feature
  = Method Objectid [Formal] Typeid AST
  | Field Objectid Typeid

data Formal = Arg Objectid Typeid

data Binding = Binding Objectid Typeid AST

data PatternMatch = PMatch Objectid Typeid AST

-- AST
data AST
  = String :<- AST
  | -- some shit i dont understand here
    FnCall [AST]
  | IfThenElse AST AST AST
  | WhileLoop AST AST
  | Statement (NonEmpty AST)
  | LetIn (NonEmpty Binding) AST
  | CaseOf (NonEmpty PatternMatch)
  | New String
  | IsVoid AST
  | AST :+: AST
  | AST :-: AST
  | AST :*: AST
  | AST :/: AST
  | Tilda AST
  | AST :<: AST
  | AST :<=: AST
  | AST :=: AST
  | Not AST
  | Parenthesised AST
  | Number ByteString SourcePos
  | Str ByteString SourcePos
  | TBool SourcePos
  | FBool SourcePos

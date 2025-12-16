module Parser.Parser
  ( MProgram (..),
    Typeid (..),
    Objectid (..),
    Program (..),
    Class (..),
    Feature (..),
    Formal (..),
    Binding (..),
    PatternMatch (..),
    AST (..),
    defaultClass,
    mkClass,
    notype,
    treefmt,
    astfmt, -- TODO: remove
    noexpr,
    astpos,
    selfObj,
  )
where

-- TODO: move formatter fns into a different module
-- and also perhaps implement a typeclass for formatting.
-- so many of the methods use VERY SIMILAR (almost identical!)
-- patterns, and especially LISTS. boilerplate could be reduced via a more generic serialisation type class.
-- in a rush rn, so will implement later

import Data.BSUtil (bytesToString)
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Parser.Position

data MProgram
  = MProgram
  { _entryPoint :: Program,
    _filename :: FilePath
  }

data Typeid = Typeid ByteString | NoType
  deriving (Show, Eq, Ord)

notype :: Typeid
notype = NoType

newtype Objectid = Objectid ByteString
  deriving (Show, Eq, Ord)

data Program = Program (NonEmpty Class) SourcePos
  deriving (Show)

data Class = ClassInherits Typeid Typeid [Feature] FilePath SourcePos
  deriving (Show)

defaultClass :: Typeid -> [Feature] -> FilePath -> SourcePos -> Class
defaultClass iden feats fp sp =
  ClassInherits iden (Typeid "Object") feats fp sp

mkClass :: Maybe Typeid -> Class -> Class
mkClass Nothing c = c
mkClass (Just inherited) (ClassInherits iden _ feats fp sp) =
  ClassInherits iden inherited feats fp sp

data Feature
  = Method Objectid [Formal] Typeid AST SourcePos
  | Field Objectid Typeid AST SourcePos
  deriving (Show, Eq, Ord)

-- useful for the ast.

data Formal = Arg Objectid Typeid SourcePos
  deriving (Show, Eq, Ord)

-- NOTE: might need source pos?

data Binding = Binding Objectid Typeid AST
  deriving (Show, Eq, Ord)

data PatternMatch = PMatch Objectid Typeid AST SourcePos
  deriving (Show, Eq, Ord)

-- AST
-- TODO: just inject the source pos directly into each node...
-- otherwise ctors such as Add, etc would have to store
-- an ADTInfo value bc of positions and stuff
data AST
  = Assign Typeid Objectid AST SourcePos
  | IfThenElse Typeid AST AST AST SourcePos -- ok
  | WhileLoop Typeid AST AST SourcePos -- ok
  | Statement Typeid (NonEmpty AST) SourcePos -- ok
  | LetIn Typeid (NonEmpty Binding) AST SourcePos -- ok
  | CaseOf Typeid AST (NonEmpty PatternMatch) SourcePos
  | New Typeid Typeid SourcePos -- ok
  | IsVoid Typeid AST SourcePos -- ok
  | Add Typeid AST AST SourcePos
  | Sub Typeid AST AST SourcePos
  | Mul Typeid AST AST SourcePos
  | Div Typeid AST AST SourcePos
  | Tilda Typeid AST SourcePos -- ok
  | Lt Typeid AST AST SourcePos
  | Leq Typeid AST AST SourcePos
  | Eq Typeid AST AST SourcePos
  | Not Typeid AST SourcePos -- ok
  -- TODO: remove this constructor eventually.
  -- i don't actually need it as of now, but if i were to remove it,
  -- i would need a setter for the source pos of an AST node,
  -- which is way too much boilerplate without lens/generics,
  -- so postponing this to a later stage. shouldn't cause any problems as is
  | Parenthesised AST SourcePos -- ok
  | StaticDispatch Typeid AST Typeid Objectid [AST] SourcePos
  | Dispatch Typeid AST Objectid [AST] SourcePos
  | Id Typeid Objectid SourcePos
  | Number Typeid Objectid SourcePos -- ok
  | Str Typeid Objectid SourcePos -- ok
  | Boolean Typeid Objectid SourcePos -- ok
  | NoExpr Typeid SourcePos
  deriving (Show, Eq, Ord)

-- TODO: use generics
astpos :: AST -> SourcePos
astpos = \case
  Assign _ _ _ p -> p
  IfThenElse _ _ _ _ p -> p
  WhileLoop _ _ _ p -> p
  Statement _ _ p -> p
  LetIn _ _ _ p -> p
  CaseOf _ _ _ p -> p
  New _ _ p -> p
  IsVoid _ _ p -> p
  Add _ _ _ p -> p
  Sub _ _ _ p -> p
  Mul _ _ _ p -> p
  Div _ _ _ p -> p
  Tilda _ _ p -> p
  Lt _ _ _ p -> p
  Leq _ _ _ p -> p
  Eq _ _ _ p -> p
  Not _ _ p -> p
  Parenthesised _ p -> p
  StaticDispatch _ _ _ _ _ p -> p
  Dispatch _ _ _ _ p -> p
  Id _ _ p -> p
  Number _ _ p -> p
  Str _ _ p -> p
  Boolean _ _ p -> p
  NoExpr _ p -> p

noexpr :: SourcePos -> AST
noexpr = NoExpr NoType

selfObj :: SourcePos -> AST
selfObj = Id notype (Objectid "self")

-- oh this is so much boilerplate im gonna cry
astfmt :: Indent -> AST -> String
astfmt indent (Assign ty iden ast pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_assign"
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ astfmt indent' ast
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
-- TODO:
astfmt indent (IfThenElse ty b t f pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_cond"
        ++ newline
        ++ astfmt indent' b
        ++ newline
        ++ astfmt indent' t
        ++ newline
        ++ astfmt indent' f
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (WhileLoop ty cond body pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_loop"
        ++ newline
        ++ astfmt indent' cond
        ++ newline
        ++ astfmt indent' body
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Statement ty asts pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_block"
        ++ newline
        ++ intercalate newline (map (astfmt indent') $ toList asts)
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
-- TODO: this is more complex than i thought.
-- they're treated as nested let bindings...
astfmt indent (LetIn ty binds body pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_let"
        ++ newline
        ++ intercalate newline (map (bindfmt indent') $ toList binds)
        ++ newline
        ++ astfmt indent' body
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (CaseOf ty ast pms pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_typcase"
        ++ newline
        ++ astfmt indent' ast
        ++ concatMap ((newline ++) . pmatchfmt indent') (toList pms)
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (New ty newty pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_new"
        ++ newline
        ++ typefmt indent' newty
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (IsVoid ty ast pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_isvoid"
        ++ newline
        ++ astfmt indent' ast
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Add ty lhs rhs pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_plus"
        ++ newline
        ++ astfmt indent' lhs
        ++ newline
        ++ astfmt indent' rhs
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Sub ty lhs rhs pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_sub"
        ++ newline
        ++ astfmt indent' lhs
        ++ newline
        ++ astfmt indent' rhs
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Mul ty lhs rhs pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_mul"
        ++ newline
        ++ astfmt indent' lhs
        ++ newline
        ++ astfmt indent' rhs
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Div ty lhs rhs pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_divide"
        ++ newline
        ++ astfmt indent' lhs
        ++ newline
        ++ astfmt indent' rhs
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Tilda ty ast pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_neg"
        ++ newline
        ++ astfmt indent' ast
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Not ty ast pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_not"
        ++ newline
        ++ astfmt indent' ast
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Lt ty lhs rhs pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_lt"
        ++ newline
        ++ astfmt indent' lhs
        ++ newline
        ++ astfmt indent' rhs
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Leq ty lhs rhs pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_le"
        ++ newline
        ++ astfmt indent' lhs
        ++ newline
        ++ astfmt indent' rhs
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Eq ty lhs rhs pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_eq"
        ++ newline
        ++ astfmt indent' lhs
        ++ newline
        ++ astfmt indent' rhs
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Parenthesised ast pos) = astfmt indent ast
astfmt indent (Id ty iden pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_object"
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (StaticDispatch ty obj ty2 iden args pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_static_dispatch"
        ++ newline
        ++ astfmt indent' obj
        ++ newline
        ++ typefmt indent' ty2
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ indent'
        ++ "("
        ++ concatMap ((newline ++) . astfmt indent') args
        ++ newline
        ++ indent'
        ++ ")"
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Dispatch ty obj iden args pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_dispatch"
        ++ newline
        ++ astfmt indent' obj
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ indent'
        ++ "("
        ++ concatMap ((newline ++) . astfmt indent') args
        ++ newline
        ++ indent'
        ++ ")"
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Number ty iden pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_int"
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Str ty (Objectid iden) pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_str"
        ++ newline
        ++ indent'
        ++ show iden
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (Boolean ty iden pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_bool"
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ indent
        ++ ":"
        ++ typefmt " " ty
astfmt indent (NoExpr ty pos) =
  posfmt indent pos
    ++ astnode indent "_no_expr"
    ++ newline
    ++ indent
    ++ ":"
    ++ typefmt " " ty

type Indent = String

-- newlines and indentation
newline :: String
newline = "\n"

idInc :: Indent -> Indent
idInc indent = "  " ++ indent

nlIndent :: Indent -> String -> String
nlIndent indent str = newline ++ indent ++ str

nlIndentB :: Indent -> ByteString -> String
nlIndentB indent bstr =
  nlIndent indent $ bytesToString bstr

-- program formatters

astnode :: Indent -> String -> String
astnode indent name = newline ++ indent ++ name

posfmt :: Indent -> SourcePos -> String
posfmt indent pos = indent ++ "#" ++ show (_lineNumber pos)

typefmt :: Indent -> Typeid -> String
typefmt indent NoType = indent ++ "_no_type"
typefmt indent (Typeid bs) =
  indent ++ bytesToString bs

objectfmt :: Indent -> Objectid -> String
objectfmt indent (Objectid bs) =
  indent ++ bytesToString bs

formalfmt :: Indent -> Formal -> String
formalfmt indent (Arg iden ty pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_formal"
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ typefmt indent' ty

bindfmt :: Indent -> Binding -> String
bindfmt indent (Binding iden ty ast) =
  objectfmt indent iden
    ++ newline
    ++ typefmt indent ty
    ++ newline
    -- TODO: indentation?
    ++ astfmt indent ast

pmatchfmt :: Indent -> PatternMatch -> String
pmatchfmt indent (PMatch iden ty ast pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_branch"
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ typefmt indent' ty
        ++ newline
        ++ astfmt indent' ast

featurefmt :: Indent -> Feature -> String
featurefmt indent (Field iden ty val pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_attr"
        ++ newline
        ++ objectfmt indent' iden
        ++ newline
        ++ typefmt indent' ty
        ++ newline
        ++ astfmt indent' val
featurefmt indent (Method iden args ty val pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_method"
        ++ newline
        ++ objectfmt indent' iden
        ++ concatMap ((newline ++) . formalfmt indent') args
        ++ newline
        ++ typefmt indent' ty
        ++ newline
        ++ astfmt indent' val

classfmt :: Indent -> Class -> String
classfmt indent (ClassInherits iden inh feats fp pos) =
  let indent' = idInc indent
   in posfmt indent pos
        ++ astnode indent "_class"
        ++ newline
        ++ typefmt indent' iden
        ++ newline
        ++ typefmt indent' inh
        ++ newline
        ++ indent'
        ++ show fp
        ++ newline
        ++ indent'
        ++ "("
        ++ concatMap ((newline ++) . featurefmt indent') feats
        ++ newline
        ++ indent'
        ++ ")"

treefmt :: Indent -> Program -> String
treefmt indent (Program cs pos) =
  posfmt indent pos
    ++ astnode indent "_program"
    ++ newline
    ++ intercalate newline (map (classfmt $ idInc indent) $ toList cs)
    ++ newline

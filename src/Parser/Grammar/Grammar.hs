module Parser.Grammar.Grammar where

-- NOTE: <- has its own precedence...
-- not sure how this plays into my grammar tbh.
-- well, due to the nature of my grammar, it consumes
-- as much input as possible to the right of it, so all is well
-- comparison operators do not associate.
-- TODO: encode precedence of all operators

import Control.Applicative (empty, (<|>))
import Data.List.NonEmpty
import Data.Maybe (fromJust)
import Prelude hiding (div, not)

import qualified Data.ByteString as B
import qualified Data.Map as M

import Lexer.Lexer (LexInfo (..), Lexeme (..), LexemeTag, Lexemes)
import Parser.Core (Parser (runParser))
import Parser.Grammar.Pratt
import Parser.Parser
import Parser.Position (SourcePos, initial)

import qualified Lexer.Lexer as L
import qualified Parser.Core as P
import qualified Parser.Position as Pos

-- combinators

lexcmp :: LexInfo -> LexInfo -> Bool
lexcmp lexeme (LexInfo (Lexeme tag2 _) _) =
  tagcmp tag2 lexeme

tagcmp :: LexemeTag -> LexInfo -> Bool
tagcmp tag (LexInfo (Lexeme tag' _) _) = tag == tag'

lsat :: LexemeTag -> P.Parser String Lexemes LexInfo
lsat tag = P.sat $ tagcmp tag

named
  :: LexemeTag -> (B.ByteString -> Pos.SourcePos -> a) -> P.Parser String Lexemes a
named tag ctor = do
  LexInfo (Lexeme _ value) pos <- lsat tag
  pure $ ctor (fromJust value) pos

extractPos :: LexInfo -> SourcePos
extractPos (LexInfo _ pos) = pos

-- objectid parser

objp :: P.Parser String Lexemes Objectid
objp = named L.OBJECTID $ const . Objectid

-- typeid parser

typp :: P.Parser String Lexemes Typeid
typp = named L.TYPEID $ const . Typeid

-- program parser

program :: FilePath -> P.Parser String Lexemes Program
program fp = do
  c <- class' fp <* lsat L.SEMICOLON
  cs <- P.many $ class' fp <* lsat L.SEMICOLON
  (Program (c :| cs) . extractPos <$> P.peek) <* lsat L.EOF

-- class parser

class' :: FilePath -> P.Parser String Lexemes Class
class' fp = do
  _ <- lsat L.CLASS
  tyIden <- typp
  inh <- P.zeroOrOne $ lsat L.INHERITS >> typp
  _ <- lsat L.LBRACE
  feats <- P.many $ feature <* lsat L.SEMICOLON
  _ <- lsat L.RBRACE
  mkClass inh . defaultClass tyIden feats fp . extractPos <$> P.peek

-- feature

-- | one or more comma-separated values
commaSep :: P.Parser String Lexemes a -> P.Parser String Lexemes (NonEmpty a)
commaSep p = do
  x <- p
  xs <- P.many $ lsat L.COMMA >> p
  pure $ x :| xs

method :: P.Parser String Lexemes Feature
method = do
  iden <- objp
  _ <- lsat L.LPAREN
  forms <- concatMap toList <$> P.many (commaSep formal)
  _ <- lsat L.RPAREN
  _ <- lsat L.COLON
  tyIden <- typp
  _ <- lsat L.LBRACE
  expr <- ast
  pos <- extractPos <$> P.peek
  _ <- lsat L.RBRACE
  pure $ Method iden forms tyIden expr pos

-- TODO:
field :: P.Parser String Lexemes Feature
field = do
  Binding obj ty expr pos <- binding
  pure $ case expr of
    Nothing -> AttrNoInit obj ty pos
    Just expr' -> AttrInit obj ty expr' pos

feature :: P.Parser String Lexemes Feature
feature = method <|> field

-- formal

formal :: P.Parser String Lexemes Formal
formal = do
  iden <- objp
  _ <- lsat L.COLON
  pos <- extractPos <$> P.peek
  tyIden <- typp
  pure $ Arg iden tyIden pos

-- binding

binding :: P.Parser String Lexemes Binding
binding = do
  Arg iden tyIden pos <- formal
  expr <- P.zeroOrOne $ lsat L.ASSIGN >> ast
  pure $ case expr of
    Nothing -> Binding iden tyIden expr pos
    Just expr' -> Binding iden tyIden expr (astpos expr')

-- pattern matches

pmatch :: P.Parser String Lexemes PatternMatch
pmatch = do
  Arg iden tyIden _ <- formal
  _ <- lsat L.DARROW
  expr <- ast
  pos <- extractPos <$> lsat L.SEMICOLON
  pure $ PMatch iden tyIden expr pos

pmatches :: P.Parser String Lexemes (NonEmpty PatternMatch)
pmatches = do
  pm <- pmatch
  pms <- P.many pmatch
  pure $ pm :| pms

-- AST parsers

id :: P.Parser String Lexemes AST
id = named L.OBJECTID $ Id notype . Objectid

int :: P.Parser String Lexemes AST
int = named L.INT_CONST $ Number notype . Objectid

bool :: P.Parser String Lexemes AST
bool = named L.BOOL_CONST $ Boolean notype . Objectid

str :: P.Parser String Lexemes AST
str = named L.STR_CONST $ Str notype . Objectid

parenthesised :: P.Parser String Lexemes AST
parenthesised = do
  _ <- lsat L.LPAREN
  res <- ast
  pos <- extractPos <$> lsat L.RPAREN
  pure $ Parenthesised res pos

not :: P.Parser String Lexemes AST
not = do
  _ <- lsat L.NOT
  res <- ast
  pure $ Not notype res $ astpos res

isvoid :: P.Parser String Lexemes AST
isvoid = do
  _ <- lsat L.ISVOID
  res <- ast
  pure $ IsVoid notype res $ astpos res

tilda :: P.Parser String Lexemes AST
tilda = do
  _ <- lsat L.TILDA
  res <- ast
  pure $ Tilda notype res $ astpos res

-- honestly, took me a hot minute to derive this,
-- but it did make me very happy in the end.
-- point-free programming ftw
assign :: P.Parser String Lexemes AST
assign = do
  -- (objp <* lsat L.ASSIGN >>= (<&>) ast . Assign notype) <*> (extractPos <$> P.peek)
  obj <- objp
  _ <- lsat L.ASSIGN
  res <- ast
  pure $ Assign notype obj res $ astpos res

ifthenelse :: P.Parser String Lexemes AST
ifthenelse = do
  _ <- lsat L.IF
  p <- ast
  _ <- lsat L.THEN
  t <- ast
  _ <- lsat L.ELSE
  f <- ast
  pos <- extractPos <$> lsat L.FI
  pure $ IfThenElse notype p t f pos

whileloop :: P.Parser String Lexemes AST
whileloop = do
  _ <- lsat L.WHILE
  cond <- ast
  _ <- lsat L.LOOP
  body <- ast
  pos <- extractPos <$> lsat L.POOL
  pure $ WhileLoop notype cond body pos

-- do-syntax is much cleaner :D
-- leaving this monument of a function definition
-- because although it is one of the most beautiful things i have
-- ever seen in haskell, it doesn't work :(

-- statement :: P.Parser String Lexemes AST
-- statement =
-- ( Statement notype
-- <$> let exprp = ast <* lsat L.SEMICOLON
-- in lsat L.LBRACE *> liftA2 (:|) exprp (P.many exprp) <* lsat L.RBRACE
-- )
-- <*> (extractPos <$> P.peek)

statement :: P.Parser String Lexemes AST
statement = do
  _ <- lsat L.LBRACE
  res <- fromList <$> P.many1 (ast <* lsat L.SEMICOLON)
  pos <- extractPos <$> lsat L.RBRACE
  pure $ Statement notype res pos

bind :: P.Parser String Lexemes (Objectid, Typeid, Maybe AST, SourcePos)
bind = do
  iden <- objp
  _ <- lsat L.COLON
  noinitPos <- extractPos <$> P.peek
  ty <- typp
  expr <- P.zeroOrOne $ lsat L.ASSIGN >> ast
  pure $ case expr of
    Nothing -> (iden, ty, expr, noinitPos)
    Just expr' -> (iden, ty, expr, astpos expr')

-- a wrapper for let expressions
letin :: P.Parser String Lexemes AST
letin = do
  _ <- lsat L.LET
  binds <- commaSep bind
  _ <- lsat L.IN
  body <- ast
  pure $ foldr buildAST body binds
 where
  buildAST (iden, ty, Just expr, sp) xs =
    LetInit notype (iden, ty, expr) xs sp
  buildAST (iden, ty, Nothing, sp) xs =
    LetNoInit notype (iden, ty) xs sp

caseof :: P.Parser String Lexemes AST
caseof = do
  _ <- lsat L.CASE
  expr <- ast
  _ <- lsat L.OF
  pms <- pmatches
  pos <- extractPos <$> lsat L.ESAC
  pure $ CaseOf notype expr pms pos

new :: P.Parser String Lexemes AST
new = do
  -- (lsat L.NEW >> New notype <$> typp) <*> (extractPos <$> P.peek)
  _ <- lsat L.NEW
  pos <- extractPos <$> P.peek
  ty <- typp
  pure $ New notype ty pos

-- i somehow made this left recursive
staticDispatch :: AST -> P.Parser String Lexemes AST
staticDispatch obj = do
  _ <- lsat L.AT
  ty <- typp
  _ <- lsat L.DOT
  iden <- objp
  _ <- lsat L.LPAREN
  args <- concatMap toList <$> P.many (commaSep ast)
  pos <- extractPos <$> lsat L.RPAREN
  pure $ StaticDispatch notype obj ty iden args pos

-- this also accepts a tree lma0. TODO: FIX
dispatch :: P.Parser String Lexemes AST
dispatch = do
  selfPos <- extractPos <$> P.peek
  iden <- objp
  _ <- lsat L.LPAREN
  args <- concatMap toList <$> P.many (commaSep ast)
  pos <- extractPos <$> lsat L.RPAREN
  pure $ Dispatch notype (selfObj selfPos) iden args pos

neg :: P.Parser String Lexemes AST
neg = do
  _ <- lsat L.TILDA
  pos <- extractPos <$> P.peek
  expr <- ast
  pure $ Tilda notype expr pos

type OperationP = P.Parser String Lexemes (AST -> AST -> AST)

add :: OperationP
add = lsat L.ADD >> pure (\lhs rhs -> Add notype lhs rhs (astpos rhs))

sub :: OperationP
sub = lsat L.SUB >> pure (\lhs rhs -> Sub notype lhs rhs (astpos rhs))

mul :: OperationP
mul = lsat L.MUL >> pure (\lhs rhs -> Mul notype lhs rhs (astpos rhs))

div :: OperationP
div = lsat L.DIV >> pure (\lhs rhs -> Div notype lhs rhs (astpos rhs))

lt :: OperationP
lt = lsat L.LT >> pure (\lhs rhs -> Lt notype lhs rhs (astpos rhs))

leq :: OperationP
leq = lsat L.LE >> pure (\lhs rhs -> Leq notype lhs rhs (astpos rhs))

eq :: OperationP
eq = lsat L.EQ >> pure (\lhs rhs -> Eq notype lhs rhs (astpos rhs))

prec :: P.Parser String Lexemes AST
prec = P.chain1 convert bpInfo (0, 1) ast' $ P.choice [add, sub, mul, div, lt, leq, eq]

ast' :: P.Parser String Lexemes AST
ast' =
  foldr1
    (<|>)
    [ int
    , bool
    , str
    , parenthesised
    , dispatch
    , not
    , tilda
    , assign
    , ifthenelse
    , whileloop
    , statement
    , letin
    , new
    , isvoid
    , caseof
    , Parser.Grammar.Grammar.id
    ]

-- TODO: add dynamic dispatch to this
lr :: AST -> P.Parser String Lexemes AST
lr obj = (staticDispatch obj >>= lr) <|> pure obj

-- some sort of left factoring
ast :: P.Parser String Lexemes AST
ast = do
  base <- prec <|> ast'
  lr base

parse :: FilePath -> Lexemes -> Either String Program
parse fp lexemes = fst <$> runParser (program fp) (initial lexemes)

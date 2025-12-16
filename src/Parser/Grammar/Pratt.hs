module Parser.Grammar.Pratt where

import qualified Data.Map as M
import qualified Lexer.Lexer as L
import qualified Parser.Parser as P
import Parser.Position (SourcePos)

type BindingPower = (Int, Int)

data PrattPrec = PrattPrec
  { _bindingPower :: BindingPower,
    _ctor :: P.Typeid -> P.AST -> P.AST -> SourcePos -> P.AST
  }

-- TODO: <- ?

bpInfo :: M.Map L.LexemeTag PrattPrec
bpInfo =
  M.fromList
    [ (L.ADD, PrattPrec (30, 31) P.Add),
      (L.SUB, PrattPrec (30, 31) P.Sub),
      (L.MUL, PrattPrec (40, 41) P.Mul),
      (L.DIV, PrattPrec (40, 41) P.Div),
      (L.LT, PrattPrec (20, 20) P.Lt),
      (L.LE, PrattPrec (20, 20) P.Leq),
      (L.EQ, PrattPrec (20, 20) P.Eq)
    ]

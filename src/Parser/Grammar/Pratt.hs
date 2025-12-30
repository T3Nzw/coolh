module Parser.Grammar.Pratt where

import qualified Data.Map as M

import qualified Lexer.Lexer as L

type BindingPower = (Int, Int)

bpInfo :: M.Map L.LexemeTag BindingPower
bpInfo =
  M.fromList
    [ (L.ADD, (30, 31))
    , (L.SUB, (30, 31))
    , (L.MUL, (40, 41))
    , (L.DIV, (40, 41))
    , (L.LT, (20, 20))
    , (L.LE, (20, 20))
    , (L.EQ, (20, 20))
    ]

-- makes me want to gouge my eyes out
convert :: [L.LexInfo] -> L.LexemeTag
convert [lexeme] = L.extractTag lexeme
convert _ = error "oh no"

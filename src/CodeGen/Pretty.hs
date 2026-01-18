module CodeGen.Pretty where

import CodeGen.Instr (Instr (Mark))

type Indent = String

ifmt :: Indent -> [Instr] -> String
ifmt indent is = go indent is
 where
  go _ [] = ""
  go indent' [i] = indent' ++ show i
  go indent' (h : t) =
    indent'
      ++ show h
      ++ "\n"
      ++ case h of
        Mark _ -> ifmt (indent ++ "  ") t
        _ -> ifmt indent t

-- >>> ifmt "" [Addi T1 A0 (Imm 42), Mark (Label 1), Add A0 T1 A0]
-- "addi t1 a0 42\nL1:\n  add a0 t1 a0"

module CodeGen.Monad where

import Control.Monad.State

import CodeGen.Instr

-- TODO: perhaps refactor to use fused effects/polysemy
-- at some point (not sure if i even need them,
-- but i learned that they exist, so now i wanna use them :) )

-- heavily inspired by this:
-- https://github.com/tomahawkins/asm-dsl-example/blob/master/Asm.hs

data CodeGenState = CodeGenState
  { _lblCnt :: Int
  , _instructions :: [Instr]
  }

type CodeGen = State CodeGenState

addInstr :: Instr -> CodeGen ()
addInstr instr = modify (\cg@(CodeGenState _ is) -> cg{_instructions = is ++ [instr]})

lw :: Reg -> OffsetReg -> CodeGen ()
lw reg offReg = addInstr $ Lw reg offReg

sw :: Reg -> OffsetReg -> CodeGen ()
sw reg offReg = addInstr $ Sw reg offReg

li :: Reg -> Imm -> CodeGen ()
li reg imm = addInstr $ Li reg imm

add :: Reg -> Reg -> Reg -> CodeGen ()
add reg1 reg2 reg3 = addInstr $ Add reg1 reg2 reg3

addi :: Reg -> Reg -> Imm -> CodeGen ()
addi reg1 reg2 imm = addInstr $ Addi reg1 reg2 imm

sub :: Reg -> Reg -> Reg -> CodeGen ()
sub reg1 reg2 reg3 = addInstr $ Sub reg1 reg2 reg3

subi :: Reg -> Reg -> Imm -> CodeGen ()
subi reg1 reg2 imm = addInstr $ Subi reg1 reg2 imm

mul :: Reg -> Reg -> Reg -> CodeGen ()
mul reg1 reg2 reg3 = addInstr $ Mul reg1 reg2 reg3

muli :: Reg -> Reg -> Imm -> CodeGen ()
muli reg1 reg2 imm = addInstr $ Muli reg1 reg2 imm

div :: Reg -> Reg -> Reg -> CodeGen ()
div reg1 reg2 reg3 = addInstr $ Div reg1 reg2 reg3

divi :: Reg -> Reg -> Imm -> CodeGen ()
divi reg1 reg2 imm = addInstr $ Divi reg1 reg2 imm

beq :: Reg -> Reg -> Label -> CodeGen ()
beq reg1 reg2 lbl = addInstr $ Beq reg1 reg2 lbl

j :: Label -> CodeGen ()
j lbl = addInstr $ J lbl

jal :: GLabel -> CodeGen ()
jal glbl = addInstr $ Jal glbl

label :: CodeGen Label
label = do
  CodeGenState lbl instr <- get
  put (CodeGenState (lbl + 1) instr)
  pure $ Label lbl

(-:) :: Label -> CodeGen a -> CodeGen a
lbl -: cg = do
  addInstr $ Mark lbl
  cg

push4 :: CodeGen ()
push4 = addi sp sp $ Imm $ -4

pop4 :: CodeGen ()
pop4 = addi sp sp $ Imm 4

assemble :: CodeGen () -> [Instr]
assemble cg = _instructions $ execState cg $ CodeGenState{_lblCnt = 0, _instructions = []}

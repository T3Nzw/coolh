module CodeGen.Instr where

-- https://projectf.io/posts/riscv-cheat-sheet/

newtype Imm = Imm Int

instance Show Imm where
  show (Imm i) = show i

imm :: Int -> Imm
imm = Imm

newtype Label = Label Int
newtype GLabel = GLabel String

instance Show Label where
  show (Label i) = "_L" ++ show i

instance Show GLabel where
  show (GLabel l) = l

data Reg
  = A0
  | T1
  | SP
  deriving Eq

instance Show Reg where
  show A0 = "a0"
  show T1 = "t1"
  show SP = "sp"

a0, t1, sp :: Reg
a0 = A0
t1 = T1
sp = SP

data OffsetReg = OffsetReg {_offset :: Int, _reg :: Reg}

off :: Int -> Reg -> OffsetReg
offset `off` reg = OffsetReg offset reg

instance Show OffsetReg where
  show (OffsetReg offset reg) = show offset ++ "(" ++ show reg ++ ")"

data Instr
  = -- moving from/to registers
    Lw Reg OffsetReg
  | Sw Reg OffsetReg
  | Li Reg Imm
  | -- arithmetic
    Add Reg Reg Reg
  | Addi Reg Reg Imm
  | Sub Reg Reg Reg
  | Mul Reg Reg Reg
  | Div Reg Reg Reg
  | Neg Reg Reg
  | -- logical
    Not Reg Reg
  | -- branching
    Beq Reg Reg Label
  | Beqz Reg Imm
  | Blt Reg Reg Imm
  | Ble Reg Reg Imm
  | -- jumps
    J Label
  | Jal GLabel
  | Ret
  | -- labels
    Mark Label

instance Show Instr where
  show (Lw reg offReg) =
    "lw " ++ show reg ++ " " ++ show offReg
  show (Sw reg offReg) =
    "sw " ++ show reg ++ " " ++ show offReg
  show (Li reg imm) =
    "li " ++ show reg ++ " " ++ show imm
  show (Add reg1 reg2 reg3) =
    "add " ++ show reg1 ++ " " ++ show reg2 ++ " " ++ show reg3
  show (Addi reg1 reg2 imm) =
    "addi " ++ show reg1 ++ " " ++ show reg2 ++ " " ++ show imm
  show (Sub reg1 reg2 reg3) =
    "sub " ++ show reg1 ++ " " ++ show reg2 ++ " " ++ show reg3
  show (Mul reg1 reg2 reg3) =
    "mul " ++ show reg1 ++ " " ++ show reg2 ++ " " ++ show reg3
  show (Div reg1 reg2 reg3) =
    "div " ++ show reg1 ++ " " ++ show reg2 ++ " " ++ show reg3
  show (Neg reg1 reg2) =
    "neg " ++ show reg1 ++ " " ++ show reg2
  show (Beq reg1 reg2 lbl) =
    "beq " ++ show reg1 ++ " " ++ show reg2 ++ " " ++ show lbl
  show (J lbl) =
    "j " ++ show lbl
  show (Jal glbl) =
    "jal " ++ show glbl
  show Ret = "ret"
  show (Mark lbl) =
    show lbl ++ ":"

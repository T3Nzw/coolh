module CodeGen.Emit where

import Prelude hiding (div)

import CodeGen.Instr
import CodeGen.Monad
import Semantic.TypedAST

class Emittable a where
  emit :: a -> CodeGen ()

instance Emittable TypedExpr where
  emit (TypedExpr _ ast) = emit ast

instance Emittable TypedAST where
  emit (TBoolean True) = do
    li a0 $ imm 1
    li t1 $ imm 1
  emit (TBoolean False) = do
    li a0 $ imm 0
    li t1 $ imm 1
  emit (TNumber n) = li A0 $ imm n
  emit (TAdd lhs rhs) = do
    emit lhs

    sw a0 $ 0 `off` sp
    push4

    emit rhs

    lw t1 $ 4 `off` sp
    add a0 t1 a0
    pop4
  emit (TSub lhs rhs) = do
    emit lhs

    sw a0 $ 0 `off` sp
    push4

    emit rhs

    lw t1 $ 4 `off` sp
    sub a0 t1 a0
    pop4
  emit (TMul lhs rhs) = do
    emit lhs

    sw a0 $ 0 `off` sp
    push4

    emit rhs

    lw t1 $ 4 `off` sp
    mul a0 t1 a0
    pop4
  emit (TDiv lhs rhs) = do
    emit lhs

    sw a0 $ 0 `off` sp
    push4

    emit rhs

    lw t1 $ 4 `off` sp
    div a0 t1 a0
    pop4
  emit (TIfThenElse b t f) = do
    emit b

    flbl <- label
    tlbl <- label
    endif <- label

    beq a0 t1 tlbl

    flbl -: do
      emit f
      j endif

    tlbl -: do
      emit t

    endif -: pure ()

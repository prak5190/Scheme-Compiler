
module CompilerHs.GenerateX86_64 where

import FrameworkHs.GenGrammars.L41FlattenProgram
import FrameworkHs.Helpers
import FrameworkHs.Prims

generateX86_64 :: P423Config -> Prog -> Out
generateX86_64 c (Code ls s) =
  do emitEntry c
     mapM statement ls
     statement s
     emitExit c
     done

statement :: Statement -> Out
statement s = case s of
  Set1 v t       -> set v t
  Set2 _ b t1 t2 -> emitOp3 (binop b) (format t2) (format t1)
  Jump t         -> jump t
  LabelS l       -> emitLabelLabel l

set :: Var -> Triv -> Out
set v t = case t of
  Var _v     -> mov
  Integer _i -> mov
  LabelT _l  -> leaq (format t) (format v)
  where
    mov = movq (format t) (format v)

jump :: Triv -> Out
jump t = case t of
  Var v     -> jmp (format v)
  Integer i -> jmp (format i)
  LabelT l  -> jmpL l
  where
    jmp = emitJump "jmp"
    jmpL = emitJumpLabel "jmp"

instance X86Print Triv where
  format (Var v) = format v
  format (Integer i) = format i
  format (LabelT l) = format l

instance X86Print Var where
  format (Reg r) = format r
  format (Disp d) = format d

binop :: Binop -> String
binop b = case b of
  ADD    -> "addq"
  SUB    -> "subq"
  MUL    -> "imulq"
  LOGAND -> "andq"
  LOGOR  -> "orq"
  SRA    -> "sarq"

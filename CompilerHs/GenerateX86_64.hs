
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
  Set1 (Reg r) i             -> movq i r
  Set2 (Reg r1) (Reg r2)     -> movq r2 r1
  Set3 (Reg r) b _ i         -> emitOp3 (binop b) i r
  Set4 (Reg r1) b _ (Reg r2) -> emitOp3 (binop b) r2 r1

binop :: Binop -> String
binop b = case b of
  ADD -> "addq"
  SUB -> "subq"
  MUL -> "imulq"
  LOGAND -> "andq"
  LOGOR  -> "orq"
  SRA    -> "sarq"


module CompilerHs.GenerateX86_64 (generateX86_64) where

import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.Helpers
import FrameworkHs.Prims

-- | Like the name says, emit x86 assembly code to stdout that
-- implements the semantics of our Scheme-like intermediate
-- representation.
generateX86_64 :: Prog -> Gen
generateX86_64 (Begin ls s) =
  do emitEntry 
     mapM statement ls
     statement s
     emitExit 

statement :: Statement -> Gen
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

module CompilerHs.GenerateX86_64 where

import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.Helpers
import FrameworkHs.Prims
import System.IO (Handle)

generateX86_64 :: P423Config -> Prog -> Handle -> IO ()
generateX86_64 c (Begin ls s) h =
  do emitOp2 h ".globl" "_scheme_entry"
     emitLabel h "_scheme_entry"
     mapM (statement h) ls
     statement h s
     emitOp1 h "ret"

statement :: Handle -> Statement -> IO ()
statement h s = case s of
  Set1 (Reg r) i             -> movq h i r
  Set2 (Reg r1) (Reg r2)     -> movq h r2 r1
  Set3 (Reg r) b _ i         -> emitOp3 h (binop b) i r
  Set4 (Reg r1) b _ (Reg r2) -> emitOp3 h (binop b) r2 r1

binop :: Binop -> String
binop b = case b of
  ADD -> "addq"
  SUB -> "subq"
  MUL -> "imulq"

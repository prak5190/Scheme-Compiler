
module CompilerHs.GenerateX86_64 (generateX86_64) where

import FrameworkHs.GenGrammars.L41FlattenProgram

import FrameworkHs.Helpers
import FrameworkHs.Prims as My

-- | Like the name says, emit x86 assembly code to stdout that
-- implements the semantics of our Scheme-like intermediate
-- representation.
generateX86_64 :: Prog -> Gen
generateX86_64 (Code ls s) =
  do emitEntry
     mapM statement ls
     statement s
     emitExit 

statement :: Statement -> Gen
statement s = case s of
  Set1 l tr           -> case tr of
                           LabelT la -> leaq tr l
                           _         -> movq tr l
  Set2 l b tr1 tr2    -> emitOp3 (binop b) tr2 tr1
  If1 r tr1 tr2 l     -> do emitOp3 "cmpq" tr2 tr1
                            emitJumpLabel (relop r False) l
  If2 r tr1 tr2 l     -> do emitOp3 "cmpq" tr2 tr1
                            emitJumpLabel (relop r True) l
  Jump tr             -> jump tr
  LabelS l            -> emitLabelLabel l

jump :: Triv -> Gen
jump tr = case tr of
  LabelT l -> emitJumpLabel "jmp" l
  _        -> emitJump "jmp" tr

------------------------------------------------------------

instance X86Print Triv where
  format t = case t of
    Loc lo    -> format lo
    Integer i -> format i
    LabelT la -> format la

instance X86Print Loc where
  format l = case l of
    RegL r  -> format r
    Disp d -> format d

------------------------------------------------------------

binop :: Binop -> OpCode
binop b = case b of
  My.ADD    -> "addq"
  My.SUB    -> "subq"
  My.MUL    -> "imulq"
  My.LOGAND -> "andq"
  My.LOGOR  -> "orq"
  My.SRA    -> "sarq"

relop :: Relop -> Bool -> OpCode
relop r = case r of
  My.EQ  -> ifs "jne" "je"
  My.LT  -> ifs "jge" "jl"
  My.LTE -> ifs "jg" "jle"
  My.GT  -> ifs "jle" "jg"
  My.GTE -> ifs "jl" "jge"

------------------------------------------------------------

ifs :: a -> a -> Bool -> a
ifs x y b = case b of
  True  -> x
  False -> y

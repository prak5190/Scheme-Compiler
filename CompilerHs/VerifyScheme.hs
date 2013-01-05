module CompilerHs.VerifyScheme where

import FrameworkHs.GenGrammars.L01VerifyScheme

import FrameworkHs.Prims
import FrameworkHs.Helpers

verifyScheme :: P423Config -> Prog -> Exc Prog
verifyScheme c p@(Begin ls s) =
  do mapM statement ls
     statement s
     return p

statement :: Statement -> Exc ()
statement s = case s of
  Set1 _v i        -> assert (isInt64 i) ("Out of 64-bit range: " ++ show i)
  Set3 v1 b v2 _i  -> do assert (v1 == v2) (varMatchError v1 v2 s)
                         binop b
  Set4 v1 b v2 _v3 -> do assert (v1 == v2) (varMatchError v1 v2 s)
                         binop b
  _                -> return ()

binop :: Binop -> Exc ()
binop b = assert (elem b [ADD,MUL,SUB]) ("Anachronistic Binop: " ++ show b)

assert :: Bool -> String -> Exc ()
assert False msg = failure msg
assert True _ = return ()

varMatchError :: Var -> Var -> Statement -> String
varMatchError v1 v2 s = show v1 ++ " and " ++ show v2 ++ " must be identical in expression (" ++ show s ++ ")"

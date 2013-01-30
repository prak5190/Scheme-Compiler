module CompilerHs.VerifyScheme (verifyScheme) where

import FrameworkHs.GenGrammars.L01VerifyScheme

import FrameworkHs.Prims
import FrameworkHs.Helpers

-- | Verify that the program is valid.  We try to catch as many errors
-- as possible in this pass, so that we do not encounter errors from
-- the middle of the compiler.
verifyScheme :: P423Config -> Prog -> Prog
verifyScheme c p@(Begin ls s) = runPassM c $ 
  do mapM statement ls
     statement s
     return p

--------------------------------------------------------------------------------
-- Helpers:

statement :: Statement -> PassM ()
statement s = case s of
  Set1 _v i        -> assert (isInt64 i) ("Out of 64-bit range: " ++ show i)
  Set3 v1 b v2 _i  -> do assert (v1 == v2) (varMatchError v1 v2 s)
                         binop b
  Set4 v1 b v2 _v3 -> do assert (v1 == v2) (varMatchError v1 v2 s)
                         binop b
  _                -> return ()

binop :: Binop -> PassM ()
binop b = assert (elem b [ADD,MUL,SUB]) ("Anachronistic Binop: " ++ show b)

assert :: Bool -> String -> PassM ()
assert False msg = passFailure "VerifyScheme" msg
assert True _ = return ()

varMatchError :: Var -> Var -> Statement -> String
varMatchError v1 v2 s = show v1 ++ " and " ++ show v2 ++ " must be identical in expression (" ++ show s ++ ")"

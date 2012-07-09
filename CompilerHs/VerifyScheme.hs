module CompilerHs.VerifyScheme where

import FrameworkHs.GenGrammars.L01VerifyScheme

import FrameworkHs.Prims
import FrameworkHs.Helpers

verifyScheme :: P423Config -> Prog -> Exc Prog
verifyScheme c p@(Letrec ls t) =
  do labelsDistinct ll
     mapM_ (tail ll) tt
     tail ll t
     return p
  where (ll,tt) = unzip ls

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

varMatchError :: Effect -> String
varMatchError e = "Var and Triv1 must be identical in expression (" ++ show e ++ ")"

reg :: Triv -> Bool
reg (Var (Reg _)) = True
reg _ = False

fv :: Triv -> Bool
fv (Var (FVar _)) = True
fv _ = False

int32 :: Triv -> Bool
int32 (Int i) = isInt32 i
int32 _ = False

uInt6 :: Triv -> Bool
uInt6 (Int i) = isUInt6 i
uInt6 _ = False

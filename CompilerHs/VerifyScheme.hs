module CompilerHs.VerifyScheme where

import FrameworkHs.GenGrammars.L01VerifyScheme

import FrameworkHs.Prims
import FrameworkHs.Helpers

verifyScheme :: P423Config -> Prog -> Prog
verifyScheme c p@(Letrec ls t) = runPassM c $ 
  do labelSuffixesDistinct (map index ll)
     mapM_ (vTail ll) tt
     vTail ll t
     return p
  where (ll,tt) = unzip ls
        index (L name ind) = ind

vTail :: [Label] -> Tail -> PassM ()
vTail ll (App t) =
  case t of
    (Integer i) -> passFailure "VerifyScheme" ("Violates machine constraints: " ++ show t)
    _       -> vTriv ll t
vTail ll (Begin es t) =
  do mapM_ (vEffect ll) es
     vTail ll t

vEffect :: [Label] -> Effect -> PassM ()
vEffect ll e@(Set1 v t) =
  do assert (setConstraints v t) $ setError e
     vTriv ll t
vEffect ll e@(Set2 v b t1 t2) =
  do assert (t1 == Var v) $ varMatchError e
     assert (opSetConstraints v b t2) $ setError e

setConstraints :: Var -> Triv -> Bool
setConstraints v t = case v of
  Reg _ -> True
  FVar _ -> case t of
    (Var (Reg _)) -> True
    (Integer i)   -> isInt32 i
    _             -> False

opSetConstraints :: Var -> Binop -> Triv -> Bool
opSetConstraints v b t = case b of
  MUL -> vReg (Var v) && (vReg t || vFV t || int32 t)
  SRA -> (vReg (Var v) || vFV (Var v)) && uInt6 t
  _   -> (vReg (Var v) && (vReg t || vFV t || int32 t))
           || (vFV (Var v) && (vReg t || int32 t))

setError :: Effect -> String
setError e = "Expression violates machine constraints: (" ++ show e ++ ")"

--labelSuffixesDistinct :: [Label] -> PassM ()
labelSuffixesDistinct [] = return ()
labelSuffixesDistinct (i:is) =
  if i `elem` is
     then passFailure "VerifyScheme" ("Duplicate label suffix: " ++ show i)
     else labelSuffixesDistinct is

vTriv :: [Label] -> Triv -> PassM ()
vTriv ll t = case t of
  (Label l) -> assert (l `elem` ll) ("Unbound label: " ++ show l)
  _         -> return ()

-- statement :: Statement -> PassM ()
-- statement s = case s of
--   Set1 _v i        -> assert (isInt64 i) ("Out of 64-bit range: " ++ show i)
--   Set3 v1 b v2 _i  -> do assert (v1 == v2) (varMatchError v1 v2 s)
--                          binop b
--   Set4 v1 b v2 _v3 -> do assert (v1 == v2) (varMatchError v1 v2 s)
--                          binop b
--   _                -> return ()

-- binop :: Binop -> PassM ()
-- binop b = assert (elem b [ADD,MUL,SUB]) ("Anachronistic Binop: " ++ show b)

assert :: Bool -> String -> PassM ()
assert False msg = passFailure "VerifyScheme" msg
assert True _ = return ()

varMatchError :: Effect -> String
varMatchError e = "Var and Triv1 must be identical in expression (" ++ show e ++ ")"

vReg :: Triv -> Bool
vReg (Var (Reg _)) = True
vReg _ = False

vFV :: Triv -> Bool
vFV (Var (FVar _)) = True
vFV _ = False

int32 :: Triv -> Bool
int32 (Integer i) = isInt32 i
int32 _ = False

uInt6 :: Triv -> Bool
uInt6 (Integer i) = isUInt6 i
uInt6 _ = False

module CompilerHs.FinalizeLocations
  ( finalizeLocations
  ) where

import FrameworkHs.GenGrammars.L01VerifyScheme
import qualified FrameworkHs.GenGrammars.L36FinalizeLocations as L2

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Data.Maybe (fromJust)

type Env = [(UVar,Loc)]

finalizeLocations :: P423Config -> Prog -> L2.Prog
finalizeLocations cfg (Letrec ls b) = runPassM cfg $ do
  bodies <- mapM fBody bodies
  b <- fBody b
  return (L2.Letrec (zip labels bodies) b)
  where
    (labels,bodies) = unzip ls

fBody :: Body -> PassM L2.Tail
fBody (Locate ls t) = fTail ls t

fVar :: Env -> Var -> PassM L2.Loc
fVar env v = case v of
  UVar uv -> fLoc $ fromJust $ lookup uv env
  --UVar uv -> case (lookup uv env) of
  --  Just l -> fLoc l
  --  Nothing -> failure ("unbound uvar (should have been handled in VerifyScheme): " ++ show uv)
  Loc l   -> fLoc l

------------------------------------------------------------

fTail :: Env -> Tail -> PassM L2.Tail
fTail env ta = case ta of
  AppT tr     -> do tr <- fTriv env tr
                    return (L2.AppT tr)
  IfT p t1 t2 -> do p <- fPred env p
                    t1 <- fTail env t1
                    t2 <- fTail env t2
                    return (L2.IfT p t1 t2)
  BeginT es t -> do es <- mapM (fEffect env) es
                    t <- fTail env t
                    return (L2.BeginT es t)

fPred :: Env -> Pred -> PassM L2.Pred
fPred env pr = case pr of
  TrueP           -> return (L2.TrueP)
  FalseP          -> return (L2.FalseP)
  AppP r tr1 tr2  -> do tr1 <- fTriv env tr1
                        tr2 <- fTriv env tr2
                        return (L2.AppP r tr1 tr2)
  IfP p1 p2 p3    -> do p1 <- fPred env p1
                        p2 <- fPred env p2
                        p3 <- fPred env p3
                        return (L2.IfP p1 p2 p3)
  BeginP es p     -> do es <- mapM (fEffect env) es
                        p <- fPred env p
                        return (L2.BeginP es p)

fEffect :: Env -> Effect -> PassM L2.Effect
fEffect env ef = case ef of
  Nop              -> return (L2.Nop)
  Set1 v tr        -> do v <- fVar env v
                         tr <- fTriv env tr
                         return (L2.Set1 v tr)
  Set2 v b tr1 tr2 -> do v <- fVar env v
                         tr1 <- fTriv env tr1
                         tr2 <- fTriv env tr2
                         return (L2.Set2 v b tr1 tr2)
  IfE p e1 e2      -> do p <- fPred env p
                         e1 <- fEffect env e1
                         e2 <- fEffect env e2
                         return (L2.IfE p e1 e2)
  BeginE es e      -> do es <- mapM (fEffect env) es
                         e <- fEffect env e
                         return (L2.BeginE es e)

fTriv :: Env -> Triv -> PassM L2.Triv
fTriv env tr = case tr of
  Var v     -> do l <- fVar env v
                  return (L2.Loc l)
  Integer i -> return (L2.Integer i)
  Label l   -> return (L2.Label l)

fLoc :: Loc -> PassM L2.Loc
fLoc l = case l of
  Reg r   -> return (L2.Reg r)
  FVar fv -> return (L2.FVar fv)

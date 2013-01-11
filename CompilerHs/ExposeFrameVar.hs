module CompilerHs.ExposeFrameVar where

import qualified FrameworkHs.GenGrammars.L01VerifyScheme as L1
import qualified FrameworkHs.GenGrammars.L37ExposeFrameVar as L2

import Prelude hiding (tail)
import FrameworkHs.Prims
import FrameworkHs.Helpers

exposeFrameVar :: P423Config -> L1.Prog -> L2.Prog
exposeFrameVar c (L1.Letrec ls t) = runPassM c $ 
  do ls <- mapM (ltTuple c) ls
     t  <- tail c t
     return (L2.Letrec ls t)

ltTuple :: P423Config -> (Label,L1.Tail) -> PassM (Label,L2.Tail)
ltTuple c (l,t) =
  do t <- tail c t
     return (l,t)

tail :: P423Config -> L1.Tail -> PassM L2.Tail
tail c t = case t of
  L1.App tr     -> do tr <- triv c tr
                      return (L2.App tr)
  L1.Begin es t -> do es <- mapM (effect c) es
                      t  <- tail c t
                      return (L2.Begin es t)

effect :: P423Config -> L1.Effect -> PassM L2.Effect
effect c e = case e of
  L1.Set1 v tr        -> do v  <- var c v
                            tr <- triv c tr
                            return (L2.Set1 v tr)
  L1.Set2 v b tr1 tr2 -> do v   <- var c v
                            tr1 <- triv c tr1
                            tr2 <- triv c tr2
                            return (L2.Set2 v b tr1 tr2)

triv :: P423Config -> L1.Triv -> PassM L2.Triv
triv c tr = case tr of
  L1.Var v     -> do v <- var c v
                     return (L2.Var v)
  L1.Integer i -> return (L2.Integer i)
  L1.Label l   -> return (L2.Label l)

var :: P423Config -> L1.Var -> PassM L2.Var
var c v = case v of
  L1.Reg  r      -> return (L2.Reg r)
  L1.FVar (FV i) -> return (L2.Disp (D (framePointerRegister c) (ash wordShift i)))

module CompilerHs.FlattenProgram
  (flattenProgram)
  where

import qualified FrameworkHs.GenGrammars.L37ExposeFrameVar as L1
import qualified FrameworkHs.GenGrammars.L41FlattenProgram as L2

import Prelude hiding (tail)
import FrameworkHs.Prims
import FrameworkHs.Helpers

-- | This pass flattens programs by moving `Letrec`-bound code blocks
-- into a single, flat `Code` listing.
flattenProgram :: P423Config -> L1.Prog -> L2.Prog
flattenProgram  c (L1.Letrec ls t) = runPassM c $ 
  do ss1 <- mapM ltTuple ls
     ss2 <- tail t
     let ss =  ss2 ++ (concat ss1)
     return (L2.Code (init ss) (last ss))

ltTuple :: (Label,L1.Tail) -> PassM [L2.Statement]
ltTuple (l,t) =
  do t <- tail t
     return ((L2.LabelS l) : t)

tail :: L1.Tail -> PassM [L2.Statement]
tail t = case t of
  L1.App tr     -> do tr <- triv tr
                      return [(L2.Jump tr)]
  L1.Begin es t -> do es <- mapM effect es
                      t  <- tail t
                      return (es ++ t)

effect :: L1.Effect -> PassM L2.Statement
effect e = case e of
  L1.Set1 v tr        -> do v  <- var v
                            tr <- triv tr
                            return (L2.Set1 v tr)
  L1.Set2 v b tr1 tr2 -> do v   <- var v
                            tr1 <- triv tr1
                            tr2 <- triv tr2
                            return (L2.Set2 v b tr1 tr2)

triv :: L1.Triv -> PassM L2.Triv
triv tr = case tr of
  L1.Var v     -> do v <- var v
                     return (L2.Var v)
  L1.Integer i -> return (L2.Integer i)
  L1.Label l   -> return (L2.LabelT l)

var :: L1.Var -> PassM L2.Var
var v = case v of
  L1.Reg r  -> return (L2.Reg r)
  L1.Disp d -> return (L2.Disp d)

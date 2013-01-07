module CompilerHs.ExposeFrameVar
  ( exposeFrameVar
  , eLoc
  ) where

import FrameworkHs.GenGrammars.L36FinalizeLocations
import qualified FrameworkHs.GenGrammars.L37ExposeFrameVar as L2

import FrameworkHs.Prims
import FrameworkHs.Helpers

exposeFrameVar :: P423Config -> Prog -> Exc L2.Prog
exposeFrameVar c (Letrec ls t) = return $
  L2.Letrec (zip las $ map (eTail fpr) ts) $ eTail fpr t
  where
    (las,ts) = unzip ls
    fpr = framePointerRegister c

--exposeFrameVar c (Letrec ls t) = do
--  ts <- mapM (eTail fpr) ts
--  t <- eTail fpr t
--  return (L2.Letrec (zip las ts) t)
--  where
--    (las,ts) = unzip ls
--    fpr = framePointerRegister c

eLoc :: Reg -> Loc -> L2.Loc
eLoc fpr lo = case lo of
  Reg r       -> L2.Reg r
  FVar (FV n) -> L2.Disp $ D fpr $ ash wordShift n

--eLoc :: Reg -> Loc -> Exc L2.Loc
--eLoc fpr lo = case lo of
--  Reg r       -> return (L2.Reg r)
--  FVar (FV n) -> return (L2.Disp $ D fpr $ ash n wordShift)

------------------------------------------------------------

eTail :: Reg -> Tail -> L2.Tail
eTail fpr ta = case ta of
  AppT tr     -> L2.AppT $ eTriv fpr tr
  IfT p t1 t2 -> L2.IfT (ePred fpr p) (eTail fpr t1) (eTail fpr t2)
  BeginT es t -> L2.BeginT (map (eEffect fpr) es) (eTail fpr t)

--eTail :: Reg -> Tail -> Exc L2.Tail
--eTail fpr ta = case ta of
--  AppT tr     -> do tr <- eTriv fpr tr
--                    return (L2.AppT tr)
--  IfT p t1 t2 -> do p <- ePred fpr p
--                    t1 <- eTail fpr t1
--                    t2 <- eTail fpr t2
--                    return (L2.IfT p t1 t2)
--  BeginT es t -> do es <- mapM (eEffect fpr) es
--                    t <- eTail fpr t
--                    return (L2.BeginT es t)

ePred :: Reg -> Pred -> L2.Pred
ePred fpr pr = case pr of
  TrueP          -> L2.TrueP
  FalseP         -> L2.FalseP
  AppP r tr1 tr2 -> L2.AppP r (eTriv fpr tr1) (eTriv fpr tr2)
  IfP p1 p2 p3   -> L2.IfP (ePred fpr p1) (ePred fpr p2) (ePred fpr p3)
  BeginP es p    -> L2.BeginP (map (eEffect fpr) es) (ePred fpr p)

--ePred :: Reg -> Pred -> Exc L2.Pred
--ePred fpr pr = case pr of
--  TrueP          -> return (L2.TrueP)
--  FalseP         -> return (L2.FalseP)
--  AppP r tr1 tr2 -> do tr1 <- eTriv fpr tr1
--                       tr2 <- eTriv fpr tr2
--                       return (L2.AppP r tr1 tr2)
--  IfP p1 p2 p3   -> do p1 <- ePred fpr p1
--                       p2 <- ePred fpr p2
--                       p3 <- ePred fpr p3
--                       return (L2.IfP p1 p2 p3)
--  BeginP es p    -> do es <- mapM (eEffect fpr) es
--                       p <- ePred fpr p
--                       return (L2.BeginP es p)

eEffect :: Reg -> Effect -> L2.Effect
eEffect fpr ef = case ef of
  Nop              -> L2.Nop
  Set1 l tr        -> L2.Set1 (eLoc fpr l) (eTriv fpr tr)
  Set2 l b tr1 tr2 -> L2.Set2 (eLoc fpr l) b (eTriv fpr tr1) (eTriv fpr tr2)
  IfE p e1 e2      -> L2.IfE (ePred fpr p) (eEffect fpr e1) (eEffect fpr e2)
  BeginE es e      -> L2.BeginE (map (eEffect fpr) es) (eEffect fpr e)

--eEffect :: Reg -> Effect -> Exc L2.Effect
--eEffect fpr ef = case ef of
--  Nop              -> return (L2.Nop)
--  Set1 l tr        -> do l <- eLoc fpr l
--                         tr <- eTriv fpr tr
--                         return (L2.Set1 l tr)
--  Set2 l b tr1 tr2 -> do l <- eLoc fpr l
--                         tr1 <- eTriv fpr tr1
--                         tr2 <- eTriv fpr tr2
--                         return (L2.Set2 l b tr1 tr2)
--  IfE p e1 e2      -> do p <- ePred fpr p
--                         e1 <- eEffect fpr e1
--                         e2 <- eEffect fpr e2
--                         return (L2.IfE p e1 e2)
--  BeginE es e      -> do es <- mapM (eEffect fpr) es
--                         e <- eEffect fpr e
--                         return (L2.BeginE es e)

eTriv :: Reg -> Triv -> L2.Triv
eTriv fpr tr = case tr of
  Loc lo    -> L2.Loc $ eLoc fpr lo
  Integer i -> L2.Integer i
  Label la  -> L2.Label la

--eTriv :: Reg -> Triv -> Exc L2.Triv
--eTriv fpr tr = case tr of
--  Loc lo    -> do lo <- eLoc fpr lo
--                  return (L2.Loc lo)
--  Integer i -> return (L2.Integer i)
--  Label la  -> return (L2.Label la)

module CompilerHs.ExposeFrameVar
  ( exposeFrameVar
  , eLoc
  ) where

import FrameworkHs.GenGrammars.L36FinalizeLocations
import qualified FrameworkHs.GenGrammars.L37ExposeFrameVar as L2

import FrameworkHs.Prims
import FrameworkHs.Helpers

-- | This pass replaces FVars with displacement operands (`Disp` type).
exposeFrameVar :: P423Config -> Prog -> L2.Prog
exposeFrameVar c (Letrec ls t) = 
  L2.Letrec (zip las $ map (eTail fpr) ts) $ eTail fpr t
  where
    (las,ts) = unzip ls
    fpr = framePointerRegister c

-- | Here is where the real work occurs.
eLoc :: Reg -> Loc -> L2.Loc
eLoc fpr lo = case lo of
  RegL r       -> L2.RegL r
  FVar (FV n) -> L2.Disp $ D fpr $ ash wordShift n

------------------------------------------------------------
-- Boilerplate

eTail :: Reg -> Tail -> L2.Tail
eTail fpr ta = case ta of
  AppT tr     -> L2.AppT $ eTriv fpr tr
  IfT p t1 t2 -> L2.IfT (ePred fpr p) (eTail fpr t1) (eTail fpr t2)
  BeginT es t -> L2.BeginT (map (eEffect fpr) es) (eTail fpr t)

ePred :: Reg -> Pred -> L2.Pred
ePred fpr pr = case pr of
  TrueP          -> L2.TrueP
  FalseP         -> L2.FalseP
  AppP r tr1 tr2 -> L2.AppP r (eTriv fpr tr1) (eTriv fpr tr2)
  IfP p1 p2 p3   -> L2.IfP (ePred fpr p1) (ePred fpr p2) (ePred fpr p3)
  BeginP es p    -> L2.BeginP (map (eEffect fpr) es) (ePred fpr p)

eEffect :: Reg -> Effect -> L2.Effect
eEffect fpr ef = case ef of
  Nop              -> L2.Nop
  Set1 l tr        -> L2.Set1 (eLoc fpr l) (eTriv fpr tr)
  Set2 l b tr1 tr2 -> L2.Set2 (eLoc fpr l) b (eTriv fpr tr1) (eTriv fpr tr2)
  IfE p e1 e2      -> L2.IfE (ePred fpr p) (eEffect fpr e1) (eEffect fpr e2)
  BeginE es e      -> L2.BeginE (map (eEffect fpr) es) (eEffect fpr e)

eTriv :: Reg -> Triv -> L2.Triv
eTriv fpr tr = case tr of
  Loc lo    -> L2.Loc $ eLoc fpr lo
  Integer i -> L2.Integer i
  Label la  -> L2.Label la

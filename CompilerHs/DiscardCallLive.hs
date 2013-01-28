
module CompilerHs.DiscardCallLive
  ( discardCallLive
  ) where

import FrameworkHs.GenGrammars.L33AssignRegisters
import qualified FrameworkHs.GenGrammars.L35DiscardCallLive as T

import FrameworkHs.Prims
import FrameworkHs.Helpers

discardCallLive :: P423Config -> Prog -> T.Prog
discardCallLive _  (Letrec binds bod) =
  T.Letrec (map (\ (l,t) -> (l, eBody t)) binds) (eBody bod)

eTail :: Tail -> T.Tail
eTail tl =
  case tl of
    -- Here is the one clause that actually does something:
    AppT tr _ls -> T.AppT (eTriv tr) 
    -------------------- boilerplate -----------------
    BeginT es e -> T.BeginT (map eEffect es) (eTail e)
    IfT p c a   -> T.IfT (ePred p) (eTail c) (eTail a)

--------------------------------------------------------------------------------
-- <boilerplate>
-- The helpers in this section are ONE HUNDRED PERCENT boilerplate.
-- They just perform conversion between the source/target AST types.
--------------------------------------------------------------------------------

eBody :: Body -> T.Body
eBody (Locate locs tl) = T.Locate locs (eTail tl)

eEffect :: Effect -> T.Effect
eEffect e =
  case e of 
    Nop              -> T.Nop
    Set1 v tr        -> T.Set1 (eVar v) (eTriv tr)
    Set2 v bop t1 t2 -> T.Set2 (eVar v) bop (eTriv t1) (eTriv t2)
    IfE p e1 e2      -> T.IfE (ePred p) (eEffect e1) (eEffect e2)
    BeginE es el     -> T.BeginE (map eEffect es) (eEffect el)

ePred :: Pred -> T.Pred
ePred pr =
  case pr of
    TrueP          -> T.TrueP
    FalseP         -> T.FalseP
    IfP p c a      -> T.IfP (ePred p) (ePred c) (ePred a)
    BeginP es p    -> T.BeginP (map eEffect es) (ePred p)
    AppP rop t1 t2 -> T.AppP rop (eTriv t1) (eTriv t2)

eTriv :: Triv -> T.Triv
eTriv x = case x of
           Var v     -> T.Var (eVar v)
           Integer i -> T.Integer i
           Label l   -> T.Label l

eLoc :: Loc -> T.Loc
eLoc (RegL r) = T.RegL r
eLoc (FVar f) = T.FVar f

eVar :: Var -> T.Var
eVar (UVarV u) = T.UVarV u
eVar (Loc l)   = T.Loc (eLoc l)

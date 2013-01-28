

module CompilerHs.AssignRegisters
  ( assignRegisters
  ) where

import Data.List  (find, delete)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import FrameworkHs.GenGrammars.L32UncoverRegisterConflict
import qualified FrameworkHs.GenGrammars.L33AssignRegisters as T

import FrameworkHs.Prims (Reg(..), UVar, numRegisters, allRegisters)
import FrameworkHs.Helpers

--------------------------------------------------------------------------------

type ConflictTable = M.Map UVar (S.Set Conflict)

--------------------------------------------------------------------------------

assignRegisters :: P423Config -> Prog -> T.Prog
assignRegisters _ (Letrec binds bod) = T.Letrec binds' bod'
 where
   bod' = eBody bod
   binds' = map (\(l,b) -> (l, eBody b)) binds


eBody :: Body -> T.Body
eBody (Locals uvs ct tl) =
  let ct' = M.map S.fromList $ M.fromList ct
      homes = findHomes uvs ct'
      spills = S.difference (S.fromList uvs)
                            (S.fromList$ map fst homes)
  in if S.null spills
     then T.Locate homes (eTail tl)
     else error$"assignRegisters: unable to assign registers to every var, left over: "
                ++show (S.toList spills)

-- findHomes :: Ord t => [t] -> M.Map k (S.Set t) -> [(t, t1)]
-- findHomes :: [UVar] -> M.Map UVar (S.Set UVar) -> [(UVar, Reg)]
findHomes :: [UVar] -> ConflictTable -> [(UVar, Reg)]
findHomes vrs ct0 = lp vrs ct0
  where
    lp [] _ = []
    lp ls@(hd:_) ct =
      let pick = fromMaybe hd (find isLowDegree ls)
          isLowDegree v = S.size (ct M.! v) < numRegisters
          conflicts = ct M.! pick
          pruned = M.map (S.delete (UVarC pick)) ct
          rest = delete pick ls
          home = lp rest pruned
      in case selectRegister conflicts home of
           Nothing -> home
           Just r  -> (pick,r) : home
    

-- selectRegister :: t1 -> t2 -> Maybe Reg
selectRegister :: S.Set Conflict -> [(UVar,Reg)] -> Maybe Reg
selectRegister conflicts home =
   find (not . (`S.member` used)) allRegisters 
  where
   used = findUsed (M.fromList home) (S.toList conflicts) 

-- findUsed :: [(UVar,Reg)] -> [Conflict] -> S.Set Reg
findUsed :: M.Map UVar Reg -> [Conflict] -> S.Set Reg   
findUsed home conflicts = 
  case conflicts of
    [] -> S.empty
    RegC r : tl -> S.insert r $ findUsed home tl
    UVarC u : tl ->
      case M.lookup u home of
        Just r  -> S.insert r $ findUsed home tl
        Nothing -> findUsed home tl

--------------------------------------------------------------------------------
-- <boilerplate>
-- The helpers in this section are ONE HUNDRED PERCENT boilerplate.
-- They just perform conversion between the source/target AST types.
--------------------------------------------------------------------------------

eTail :: Tail -> T.Tail
eTail tl =
  case tl of
    BeginT es e -> T.BeginT (map eEffect es) (eTail e)
    IfT p c a   -> T.IfT (ePred p) (eTail c) (eTail a)
    AppT tr ls  -> T.AppT (eTriv tr) (map eLoc ls)

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

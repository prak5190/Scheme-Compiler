module CompilerHs.FlattenProgram
  (flattenProgram)
  where

import FrameworkHs.GenGrammars.L39ExposeBasicBlocks
import qualified FrameworkHs.GenGrammars.L41FlattenProgram as L2

import FrameworkHs.Prims
import FrameworkHs.Helpers

flattenProgram :: P423Config -> Prog -> Exc L2.Prog
flattenProgram  c (Letrec ls t) = return $ L2.Code ss s
  where
    (labels,tails) = unzip ls
    ss             = init statements
    s              = last statements
    f :: Tail -> [Label] -> [Tail] -> [L2.Statement]
    f ta las tas   = if null tas
                        then fTail Nothing ta
                        else concat
                               [ fTail (Just $ head las) ta
                               , [L2.LabelS $ head las]
                               , f (head tas) (tail las) (tail tas)
                               ]
    statements     = f t labels tails

fTail :: Maybe Label -> Tail -> [L2.Statement]
fTail nextL ta = case ta of
  App tr             |  tr `mTrEq` nextL -> []
                     |  otherwise        -> [L2.Jump $ fTriv tr]
  If r tr1 tr2 tL fL |  fL `mEq` nextL   -> [L2.If1 r (fTriv tr1) (fTriv tr2) tL]
                     |  tL `mEq` nextL   -> [L2.If2 r (fTriv tr1) (fTriv tr2) fL]
                     |  otherwise        -> [L2.If1 r (fTriv tr1) (fTriv tr2) tL
                                            ,L2.Jump $ L2.LabelT fL
                                            ]
  Begin es t         -> concat [map fEffect es, fTail nextL t]

fEffect :: Effect -> L2.Statement
fEffect ef = case ef of
  Set1 l tr        -> L2.Set1 (fLoc l) (fTriv tr)
  Set2 l b tr1 tr2 -> L2.Set2 (fLoc l) b (fTriv tr1) (fTriv tr2)

------------------------------------------------------------
-- Helpers

mTrEq :: Triv -> Maybe Label -> Bool
mTrEq tr mL = case tr of
  Label la -> la `mEq` mL
  _        -> False

mEq :: Label -> Maybe Label -> Bool
mEq l mL = case mL of
  Just l' -> l == l'
  Nothing -> False

------------------------------------------------------------
-- Boilerplate

fTriv :: Triv -> L2.Triv
fTriv tr = case tr of
  Loc lo    -> L2.Loc $ fLoc lo
  Integer i -> L2.Integer i
  Label la  -> L2.LabelT la

fLoc :: Loc -> L2.Loc
fLoc l = case l of
  Reg r  -> L2.Reg r
  Disp d -> L2.Disp d

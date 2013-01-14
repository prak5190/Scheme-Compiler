
module CompilerHs.ExposeBasicBlocks
  ( exposeBasicBlocks
  ) where

import FrameworkHs.GenGrammars.L37ExposeFrameVar
import qualified FrameworkHs.GenGrammars.L39ExposeBasicBlocks as L2

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Control.Monad.Trans.State
import Data.Int

type IndexState = State Integer

exposeBasicBlocks :: P423Config -> Prog -> L2.Prog
exposeBasicBlocks c (Letrec ls t) = flip evalState 100000 $ do
  ts <- mapM eTail tails
  let (tails,binds) = unzip ts
  (t,bind) <- eTail t
  return (L2.Letrec ((zip labels tails) ++ (concat binds) ++ bind) t)
  where
    (labels,tails) = unzip ls

eTail :: Tail -> IndexState (L2.Tail,[(Label,L2.Tail)])
eTail ta = case ta of
  AppT tr     -> return (L2.App $ eTriv tr,[])
  IfT p t1 t2 -> do (conseq,cbs) <- eTail t1
                    (altern,abs) <- eTail t2
                    cl <- newLabel "c"
                    al <- newLabel "a"
                    (p,pbs) <- ePred p cl al
                    return (p, pbs++[(cl,conseq)]++cbs++[(al,altern)]++abs)
  BeginT es t -> do (t,tbs) <- eTail t
                    (x,xbs) <- eEffects es [] t
                    return (x,xbs++tbs)

ePred :: Pred -> Label -> Label -> IndexState (L2.Tail,[(Label,L2.Tail)])
ePred pr cl al = case pr of
  TrueP          -> return (appT cl,[])
  FalseP         -> return (appT al,[])
  AppP r tr1 tr2 -> return (L2.If r (eTriv tr1) (eTriv tr2) cl al,[])
  IfP p1 p2 p3   -> do (conseq,cbs) <- ePred p2 cl al
                       (altern,abs) <- ePred p3 cl al
                       cl <- newLabel "c"
                       al <- newLabel "a"
                       (p,pbs) <- ePred p1 cl al
                       return (p,pbs++[(cl,conseq)]++cbs++[(al,altern)]++abs)
  BeginP es p    -> do (p,pbs) <- ePred p cl al
                       (x,xbs) <- eEffects es [] p
                       return (x,xbs++pbs)

eEffects :: [Effect] -> [L2.Effect] -> L2.Tail -> IndexState (L2.Tail,[(Label,L2.Tail)])
eEffects xs es t = case xs of
  [] -> return (makeBegin es t,[])
  _  -> eEffect xs' x' es t
  where
    xs' = init xs
    x'  = last xs

eEffect :: [Effect] -> Effect -> [L2.Effect] -> L2.Tail -> IndexState (L2.Tail,[(Label,L2.Tail)])
eEffect xs x es t = case x of
  Nop              -> eEffects xs es t
  Set1 l tr        -> eEffects xs ((L2.Set1 (eLoc l) (eTriv tr)) : es) t
  Set2 l b tr1 tr2 -> eEffects xs ((L2.Set2 (eLoc l) b (eTriv tr1) (eTriv tr2)) : es) t
  IfE p e1 e2      -> do cl <- newLabel "c"
                         al <- newLabel "a"
                         jl <- newLabel "j"
                         (conseq,cbs) <- eEffect [] e1 [] (appT jl)
                         (altern,abs) <- eEffect [] e2 [] (appT jl)
                         (p,pbs)      <- ePred p cl al
                         (x,xbs)      <- eEffects xs [] p
                         return (x,xbs++pbs++[(cl,conseq)]++cbs++[(al,altern)]++abs++[(jl,makeBegin es t)])
  BeginE es' e      -> eEffects (xs++es') es t

------------------------------------------------------------
-- Monadic

newLabel :: String -> IndexState Label
newLabel name = do
  i <- get
  put $ i + 1
  return (L name i)

------------------------------------------------------------
-- Boilerplate

eTriv :: Triv -> L2.Triv
eTriv tr = case tr of
  Loc lo    -> L2.Loc $ eLoc lo
  Integer i -> L2.Integer i
  Label la  -> L2.Label la

eLoc :: Loc -> L2.Loc
eLoc l = case l of
  Reg r  -> L2.Reg r
  Disp d -> L2.Disp d

------------------------------------------------------------
-- Helpers

appT :: Label -> L2.Tail
appT = L2.App . L2.Label

makeBegin :: [L2.Effect] -> L2.Tail -> L2.Tail
makeBegin es t = case es of
  [] -> t
  _  -> case t of
          L2.Begin es' t' -> makeBegin (es++es') t'
          _               -> L2.Begin es t

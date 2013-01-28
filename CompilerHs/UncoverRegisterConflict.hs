{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CompilerHs.UncoverRegisterConflict
  ( uncoverRegisterConflict )
  where

import FrameworkHs.GenGrammars.L01VerifyScheme
import qualified FrameworkHs.GenGrammars.L32UncoverRegisterConflict as T

import FrameworkHs.Prims   (UVar)
import FrameworkHs.Helpers
import Control.Monad.State
-- import Control.Monad
-- import Control.Applicative ((<$>), (<*>))

import Data.Set as S
import Data.Map as M hiding (mapMaybe) 
import Prelude as P
import Data.Maybe (mapMaybe)

--------------------------------------------------------------------------------
-- Types for local use:

type ConflictTable = Map UVar (Set T.Conflict)

-- | A little shorthand.
type LiveSet = S.Set T.Conflict

-- | A shorthand for the predominant monad we use in this pass:
type M a = State ConflictTable a

--------------------------------------------------------------------------------

uncoverRegisterConflict :: P423Config -> Prog -> T.Prog
uncoverRegisterConflict _ (Letrec binds bd) =
  T.Letrec (zip labels tails') bd'
  where
    bd' = eBody bd
    (labels,tails) = unzip binds
    tails' = P.map eBody tails

--------------------------------------------------------------------------------
-- (non-trivial) Helpers for each non-terminal in the grammar:
--------------------------------------------------------------------------------

eBody :: Body -> T.Body
eBody (Locals uvs tl) = T.Locals uvs conflicts tl'
 where
   conflicts = M.toList$   M.map S.toList tbl
   inittbl   = M.fromList$ P.map (\x -> (x,S.empty)) uvs
   (_,tbl) = runState (liveTail tl) inittbl
   tl' = eTail tl

-- | This returns a set of live variables.
liveTail :: Tail -> M LiveSet
liveTail tl =
  case tl of
    BeginT es e -> liveEffects es =<< liveTail e
    IfT p c a   -> ap2 (livePred p) (liveTail c) (liveTail a)
    AppT tr ls  -> return$ maybadd (fromTriv tr) (S.fromList$ mapMaybe fromLoc ls)

liveEffects :: [Effect] -> LiveSet -> M LiveSet
liveEffects els live =
  case els of
    [] -> return live
    ls -> do live2 <- liveEffect (last ls) live
             liveEffects (init ls) live2 

liveEffect :: Effect -> LiveSet -> M LiveSet
liveEffect e live =
  case e of
    Nop              -> return live
    IfE p e1 e2      -> ap2 (livePred p) (liveEffect e1 live)
                                         (liveEffect e2 live)
    BeginE es el     -> liveEffects es =<< liveEffect el live
--    Set1 v tr | -> -- todo: give a warning for unused vars
    Set1 v tr -> do
      let live' = maybdel (fromVar v) live
      addConflict v live'
      return (maybadd (fromTriv tr) live')
    Set2 v _ t1 t2 -> do
      let live' = maybdel (fromVar v) live
      addConflict v live'
      return (S.union (maybToSet (fromTriv t1))
                      (maybToSet (fromTriv t2)))

livePred :: Pred -> LiveSet -> LiveSet -> M LiveSet 
livePred pr tlive flive =
  case pr of
    TrueP  -> return tlive
    FalseP -> return flive
    IfP p c a -> ap2 (livePred p)
                     (livePred c tlive flive)
                     (livePred a tlive flive)
    BeginP es p -> liveEffects es =<< livePred p tlive flive
    AppP _ t1 t2 -> return$
                      S.union tlive $
                      S.union flive $
                      S.union (maybToSet (fromTriv t1))
                              (maybToSet (fromTriv t2))


--------------------------------------------------------------------------------
-- The helpers in this section are ONE HUNDRED PERCENT boilerplate.
-- They just perform conversion between the source/target AST types.
--------------------------------------------------------------------------------
eTail :: Tail -> T.Tail
eTail tl =
  case tl of
    BeginT es e -> T.BeginT (P.map eEffect es) (eTail e)
    IfT p c a   -> T.IfT (ePred p) (eTail c) (eTail a)
    AppT tr ls  -> T.AppT (eTriv tr) (P.map eLoc ls)

eEffect :: Effect -> T.Effect
eEffect e =
  case e of 
    Nop              -> T.Nop
    Set1 v tr        -> T.Set1 (eVar v) (eTriv tr)
    Set2 v bop t1 t2 -> T.Set2 (eVar v) bop (eTriv t1) (eTriv t2)
    IfE p e1 e2      -> T.IfE (ePred p) (eEffect e1) (eEffect e2)
    BeginE es el     -> T.BeginE (P.map eEffect es) (eEffect el)

ePred :: Pred -> T.Pred
ePred pr =
  case pr of
    TrueP          -> T.TrueP
    FalseP         -> T.FalseP
    IfP p c a      -> T.IfP (ePred p) (ePred c) (ePred a)
    BeginP es p    -> T.BeginP (P.map eEffect es) (ePred p)
    AppP rop t1 t2 -> T.AppP rop (eTriv t1) (eTriv t2)

eTriv :: Triv -> T.Triv
eTriv x = case x of
           Var v     -> T.Var (eVar v)
           Integer i -> T.Integer i
           Label l   -> T.Label l

eLoc :: Loc -> T.Loc
eLoc (Reg r)  = T.RegL r
eLoc (FVar f) = T.FVar f

eVar :: Var -> T.Var
eVar (UVar u) = T.UVarV u
eVar (Loc l)  = T.Loc (eLoc l)

--------------------------------------------------------------------------------
-- * Misc small helpers
--------------------------------------------------------------------------------

-- | Register a conflict in the conflict table (as a side effect).
addConflict :: Var -> LiveSet -> M ()
addConflict (Loc (FVar _)) _  = return ()
addConflict (Loc (Reg r)) set = mapM_ fn (S.toList set)
  where
    fn :: T.Conflict -> M ()
    -- Register/register conflicts don't matter
    fn (T.RegC _)   = return ()
    fn (T.UVarC uv) = modify (M.adjust (S.insert (T.RegC r)) uv)
addConflict (UVar v) set = mapM_ add (S.toList set)
  where
    add :: T.Conflict -> M ()
    add x = modify (M.adjust (S.insert x) v)

-- | Get a conflict from a Loc:
fromLoc :: Loc -> Maybe T.Conflict
fromLoc (Reg r)  = Just (T.RegC r)
fromLoc (FVar _) = Nothing

fromVar :: Var -> Maybe T.Conflict
fromVar (UVar u)       = Just (T.UVarC u)
fromVar (Loc (Reg r))  = Just (T.RegC r)
fromVar (Loc (FVar _)) = Nothing

fromTriv :: Triv -> Maybe T.Conflict
fromTriv (Var v) = fromVar v
fromTriv _       = Nothing

-- | MAYBE add to a set.
maybadd :: Ord a => Maybe a -> Set a -> Set a
maybadd Nothing s  = s
maybadd (Just x) s = S.insert x s 

-- | MAYBE delete from a set.
maybdel :: Ord a => Maybe a -> Set a -> Set a
maybdel Nothing s  = s
maybdel (Just x) s = S.delete x s 

maybToSet :: Maybe a -> Set a
maybToSet (Just x) = S.singleton x
maybToSet Nothing  = S.empty

-- | A helper for applying monadic functions.
ap2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
ap2 fn a b =
  do a' <- a
     b' <- b
     fn a' b'
                     
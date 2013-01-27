{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CompilerHs.UncoverRegisterConflict
  ( uncoverRegisterConflict
  ) where

import FrameworkHs.GenGrammars.L01VerifyScheme
import qualified FrameworkHs.GenGrammars.L32UncoverRegisterConflict as T

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Control.Monad.State
import Control.Monad

import Control.Monad.Writer

type Conflicts = [Int]

-- Here we use a writer monad to accumulate a table of conflicts.

uncoverRegisterConflict :: P423Config -> Prog -> T.Prog
uncoverRegisterConflict _ (Letrec ls bd) = a
  where
    w :: Conflicts
    (a,w) = runWriter $ do
      let (labels,tails) = unzip ls
      ts  <- mapM eBody tails
      bd' <- eBody bd
      return$ T.Letrec (zip labels ts) bd' 

eBody :: Body -> Writer Conflicts T.Body
eBody (Locals _uvs tl) =
  eTail tl
  error "finish uncoverRegisterConflict"


eTail :: Tail -> Writer Conflicts T.Tail
eTail tl =
  case tl of
    BeginT _ _ -> error "finish uncoverRegisterConflict"
    IfT _ _ _ -> error "finish uncoverRegisterConflict" 
    AppT  _ _ -> error "finish uncoverRegisterConflict" 

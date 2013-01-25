

module CompilerHs.UncoverRegisterConflict
  ( uncoverRegisterConflict
  ) where

import FrameworkHs.GenGrammars.L01VerifyScheme
import qualified FrameworkHs.GenGrammars.L32UncoverRegisterConflict as T

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Control.Monad.State
import Control.Monad

-- Here we use a writer monad to accumulate a table of conflicts.

uncoverRegisterConflict :: P423Config -> Prog -> T.Prog
uncoverRegisterConflict _ (Letrec ls bd) = flip evalState "" $ do
  let (labels,tails) = unzip ls
  ts  <- mapM eTail tails
  bd' <- eTail bd
  return$ T.Letrec (zip labels ts) bd' 

eTail = error "finish uncoverRegisterConflict"


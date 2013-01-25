

module CompilerHs.AssignRegisters
  ( assignRegisters
  ) where

import FrameworkHs.GenGrammars.L32UncoverRegisterConflict
import qualified FrameworkHs.GenGrammars.L33AssignRegisters as T

import FrameworkHs.Prims
import FrameworkHs.Helpers

assignRegisters :: P423Config -> Prog -> T.Prog
assignRegisters = undefined

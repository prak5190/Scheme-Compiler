
module CompilerHs.DiscardCallLive
  ( discardCallLive
  ) where

import FrameworkHs.GenGrammars.L33AssignRegisters
import qualified FrameworkHs.GenGrammars.L35DiscardCallLive as T

import FrameworkHs.Prims
import FrameworkHs.Helpers

discardCallLive :: P423Config -> Prog -> T.Prog
discardCallLive = error "finish discardCallLive"

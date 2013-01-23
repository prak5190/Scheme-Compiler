module CompilerHs.Compile
  ( p423Compile
  ) where

import System.IO
import System.Cmd
import System.Process
import System.Exit
import Control.Exception (throw)

import FrameworkHs.Driver
import FrameworkHs.Prims
import FrameworkHs.Helpers
import FrameworkHs.SExpReader.LispData

import FrameworkHs.ParseL01                    (parseProg)
import FrameworkHs.GenGrammars.L01VerifyScheme
import CompilerHs.VerifyScheme                 (verifyScheme)
import CompilerHs.GenerateX86_64               (generateX86_64)

import qualified Data.ByteString as B

vfs = P423Pass { pass = verifyScheme
               , passName = "verifyScheme"
               , wrapperName = "verify-scheme/wrapper"
               , trace = False
               }

p423Compile :: LispVal -> CompileM String
p423Compile l = do
  p  <- liftPassM$ parseProg l
  p  <- runPass vfs p
  assemble$ generateX86_64 p


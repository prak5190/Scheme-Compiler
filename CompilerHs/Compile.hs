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

import FrameworkHs.ParseL01
import FrameworkHs.GenGrammars.L01VerifyScheme
import CompilerHs.VerifyScheme
import CompilerHs.GenerateX86_64

assemblyCmd :: String
assemblyCmd = "cc"
assemblyArgs :: [String]
assemblyArgs = ["-m64","-o","t","t.s","Framework/runtime.c"]

vfs = P423Pass { pass = verifyScheme
               , passName = "verifyScheme"
               , wrapperName = "verify-scheme/wrapper"
               , trace = False
               }

p423Compile :: P423Config -> LispVal -> IO String
p423Compile c l = do
  p <- runPass vfs c p
  assemble $ generateX86_64 c p
  where p = case (parseProg l) of
              Left e -> throw (ASTParseException e)
              Right x -> x

------------------------------------------------------------
-- Helpers -------------------------------------------------

assemble :: Out -> IO String
assemble out =
  do withFile "t.s" WriteMode (runOut out)
     (ec,_,e) <- readProcessWithExitCode assemblyCmd assemblyArgs ""
     case ec of
       ExitSuccess   -> do res <- readProcess "./t" [] ""
                           return (chomp res)
       ExitFailure i -> throw (AssemblyFailedException e)

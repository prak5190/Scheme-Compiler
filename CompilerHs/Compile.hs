module CompilerHs.Compile
  ( defaultConf
  , defaultTestFile
  , p423Compile
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
import CompilerHs.ExposeFrameVar
import CompilerHs.FlattenProgram
import CompilerHs.GenerateX86_64

assemblyCmd :: String
assemblyCmd = "cc"
assemblyArgs :: [String]
assemblyArgs = ["-m64","-o","t","t.s","Framework/runtime.c"]

defaultTestFile :: String
defaultTestFile = "test-suite.ss"

defaultConf :: P423Config
defaultConf = P423Config
         { framePointerRegister      = RBP
         , allocationPointerRegister = RDX
         , returnAddressRegister     = R15
         , returnValueRegister       = RAX
         , parameterRegisters        = [R8,R9]
         }

vfs = P423Pass { pass = verifyScheme
               , passName = "verifyScheme"
               , wrapperName = "verify-scheme/wrapper"
               , trace = False
               }

efv = P423Pass { pass = exposeFrameVar
               , passName = "exposeFrameVar"
               , wrapperName = "expose-frame-var/wrapper"
               , trace = False
               }

flp = P423Pass { pass = flattenProgram
               , passName = "flattenProgram"
               , wrapperName = "flatten-program/wrapper"
               , trace = True
               }

p423Compile :: P423Config -> Compiler
p423Compile c l = do
  p <- runPass vfs c p
  p <- runPass efv c p
  p <- runPass flp c p
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
       ExitFailure i -> throw AssemblyFailedException

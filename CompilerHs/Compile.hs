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
import CompilerHs.GenerateX86_64

compileAssemblyCmd :: String
compileAssemblyCmd = "cc -m64 -o t t.s Framework/runtime.c"

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

p423Compile :: P423Config -> Compiler
p423Compile conf l =
  do p <- runPass vfs conf p
     assemble $ generateX86_64 conf p
  where p = case (parseProg l) of
              Left e -> throw (ASTParseException e)
              Right x -> x

------------------------------------------------------------
-- Helpers -------------------------------------------------

assemble :: (Handle -> IO ()) -> IO String
assemble f =
  do withFile "t.s" WriteMode f
     ec <- system compileAssemblyCmd
     case ec of
       ExitSuccess   -> do res <- readProcess "./t" [] ""
                           return (chomp res)
       ExitFailure i -> throw AssemblyFailedException

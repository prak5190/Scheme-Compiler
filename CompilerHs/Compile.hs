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
import CompilerHs.FinalizeLocations            (generateX86_64)
import CompilerHs.ExposeBasicBlocks            (exposeBasicBlocks)
import CompilerHs.ExposeFrameVar               (exposeFrameVar)
import CompilerHs.FlattenProgram               (flattenProgram)
import CompilerHs.GenerateX86_64               (generateX86_64)

import qualified Data.ByteString as B

assemblyCmd :: String
assemblyCmd = "cc"
assemblyArgs :: [String]
assemblyArgs = ["-m64","-o","t","t.s","Framework/runtime.c"]

vfs = P423Pass { pass = verifyScheme
               , passName = "verifyScheme"
               , wrapperName = "verify-scheme/wrapper"
               , trace = False
               }

fnl = P423Pass { pass = finalizeLocations
               , passName = "finalizeLocations"
               , wrapperName = "finalize-locations/wrapper"
               , trace = False
               }

efv = P423Pass { pass = exposeFrameVar
               , passName = "exposeFrameVar"
               , wrapperName = "expose-frame-var/wrapper"
               , trace = False
               }

ebb = P423Pass { pass = exposeBasicBlocks
               , passName = "exposeBasicBlocks"
               , wrapperName = "expose-basic-blocks/wrapper"
               , trace = False
               }

flp = P423Pass { pass = flattenProgram
               , passName = "flattenProgram"
               , wrapperName = "flatten-program/wrapper"
               , trace = False
               }

p423Compile :: P423Config -> LispVal -> IO String
p423Compile c l = do
  let p = runPassM c $ parseProg l
  p <- runPass vfs c p
  p <- runPass fnl c p
  p <- runPass efv c p
  p <- runPass ebb c p
  p <- runPass flp c p
  assemble c $ generateX86_64 p

------------------------------------------------------------
-- Helpers -------------------------------------------------

<<<<<<< HEAD
assemble :: Out -> IO String
assemble out =
  do withFile "t.s" WriteMode (runOut out)
     (ec,_,e) <- readProcessWithExitCode assemblyCmd assemblyArgs ""
     case ec of
       ExitSuccess -> readProcess "./t" [] ""
       ExitFailure i -> throw $ AssemblyFailedException e
=======
assemble :: P423Config -> Gen -> IO String
assemble c out =
  case runGenM c out of
    Left err -> error err
    Right (_,bsout) -> do  
      B.writeFile "t.s" bsout
      (ec,_,e) <- readProcessWithExitCode assemblyCmd assemblyArgs ""
      case ec of
        ExitSuccess   -> do res <- readProcess "./t" [] ""
                            return (chomp res)
        ExitFailure i -> throw (AssemblyFailedException e)
>>>>>>> a2

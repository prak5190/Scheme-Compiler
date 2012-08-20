module FrameworkHs.Driver
  ( runWrapper
  , runPass
  )
  where

import System.Process
import System.IO
import Control.Monad (when)
import Control.Exception (throw)
import FrameworkHs.SExpReader.Parser
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers

scheme :: String
scheme = "petite -q"

loadFramework :: Handle -> IO ()
loadFramework = flip hPutStrLn "(import (Framework driver) (Framework wrappers))"

-- | A `Wrapper` is the name of the language-wrapper (a
-- Scheme-identifier).  That is, the thing that makes the Scheme
-- intermediate representation directly executable in Scheme.
type WrapperName = String

type PassName = String

runWrapper :: PP a => WrapperName -> a -> IO (Exc LispVal)
runWrapper wrapper code =
  do (i,o,e,pid) <- runInteractiveCommand scheme
     loadFramework i
     hPutStrLn i $ app wrapper $ quote code
     hClose i
     eOut <- hGetContents e
     if (eOut == "")
        then (do oOut <- hGetContents o
                 terminateProcess pid
                 return $
                   (case (readExpr oOut) of
                     Left e -> Left $ show e
                     Right code -> Right code))
        else (do terminateProcess pid
                 return $ Left eOut)
  where app :: String -> String -> String
        app rator rand = "(" ++ rator ++ " " ++ rand ++ ")"
        quote :: PP a => a -> String
        quote e = app "quote" $ pp e

runPass :: PP b => P423Pass a b -> P423Config -> a -> IO b
runPass p conf code =
  case ((pass p) conf code) of
    Left e      -> throw $ PassFailureException pn e
    Right code' -> do res <- runWrapper wn code'
                      case res of
                        Left e  -> throw $ WrapperFailureException wn e
                        Right _ -> do when (trace p)
                                        (printTrace (passName p) code')
                                      return code'
  where pn = passName p
        wn = wrapperName p
        printTrace :: PP a => String -> a -> IO ()
        printTrace name code = putStrLn ("\n" ++ name ++ ": \n" ++ pp code ++ "\n")

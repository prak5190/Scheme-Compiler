module FrameworkHs.Driver
  ( runPass
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

schemeCmd :: String
schemeCmd = "petite -q"

loadFramework :: Handle -> IO ()
loadFramework = flip hPutStrLn "(import (Framework driver) (Framework wrappers))"

runWrapper :: PP a => String -> a -> IO (Exc LispVal)
runWrapper wrapper code =
  do (i,o,e,id) <- runInteractiveCommand schemeCmd
     loadFramework i
     hPutStrLn i $ app wrapper $ quote code
     hClose i
     eOut <- hGetContents e
     if (eOut == "")
        then (do oOut <- hGetContents o
                 terminateProcess id
                 return $
                   (case (readExpr oOut) of
                     Left e -> Left $ show e
                     Right code -> Right code))
        else (do terminateProcess id
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

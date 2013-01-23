{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module FrameworkHs.Driver
  -- ( runWrapper
  -- , runPass
  -- )
  where

import System.Process                  (runInteractiveCommand, terminateProcess)
import System.IO                       (stderr, hGetContents, hPutStrLn, hClose, hFlush, hIsClosed, hIsReadable,
                                        Handle, BufferMode(..), hSetBuffering)
import Control.Monad                   (when)
import Control.Monad.Reader            (runReaderT)
import Control.Exception               (throw, catch, SomeException)
import Data.ByteString                 (ByteString, hPut, hGetNonBlocking, hGetLine, concat)
import Data.ByteString.Char8           (unpack)
import Data.Monoid                     (mconcat, (<>))
import Data.String                     (IsString(..))
import Prelude as P                    hiding (concat)

import Blaze.ByteString.Builder        (Builder, toByteString)
import qualified Blaze.ByteString.Builder.Char8  as BBB

import FrameworkHs.SExpReader.Parser   (readExpr)
import FrameworkHs.SExpReader.LispData (LispVal)
import FrameworkHs.Prims               ()
import FrameworkHs.Helpers             


-- | Which Chez Scheme should we use?
scheme :: String
scheme = "petite -q --eedisable"

-- | Tell a scheme process to load the necessary libraries.
loadFramework :: Handle -> IO ()
loadFramework = flip hPutStrLn "(import (Framework driver) (Framework wrappers))"

-- | A `Wrapper` is the name of the language-wrapper (a
-- Scheme-identifier).  That is, the thing that makes the Scheme
-- intermediate representation directly executable in Scheme.
type WrapperName = String

type PassName = String

-- | An interactive scheme process that can evaluate an unlimited
-- number of expressions before being shut down (NOT threadsafe).
data SchemeProc = SchemeProc {
  eval :: ByteString -> IO LispVal,
  shutdown :: IO () }

-- TODO: Implement a timeout:
makeSchemeEvaluator :: IO SchemeProc
makeSchemeEvaluator = do
  (ip,op,ep,pid) <- runInteractiveCommand scheme
  hSetBuffering ep NoBuffering
  hSetBuffering ip LineBuffering
  loadFramework ip
  let shutdown = terminateProcess pid
      -- Shutdown the child process if anything goes wrong:
      wrap io = catch io $ \e -> do
                  hPutStrLn stderr " [Exception!  Shutting down child scheme proccess ]"
                  shutdown
                  throw (e::SomeException)
      eval bstr = wrap$ do
        -- Interaction protocol:  Write expressions, read results delimited by blank lines.
        hPut ip bstr
        hPut ip $ toByteString $ app "newline" []
        hPut ip "\n"
        hFlush ip
        ----------------------------------------
        getRespose
      getRespose = do  
        err <- hGetNonBlocking ep 4096
        -- let err = ""
        if err == "" then
          do lns <- readUntilBlank op
             -- There's a race when an error occurs:
             if lns == "" then getRespose               
              else case readExpr (unpack lns) of
                    Left er   -> error $ show er
                    Right lsp -> return lsp
        else
         do error$ "Error from child scheme process:\n"++unpack err
  return$ SchemeProc { eval, shutdown } 



-- The compiler monad:
-- type CompileM = StateT CompileState PassM 
-- type CompileState = CompileState { lastresult :: Maybe ByteString, runner :: SchemeProc }


runWrapper :: PP a => WrapperName -> a -> IO LispVal
runWrapper wrapper code =
  do (i,o,e,pid) <- runInteractiveCommand scheme
     loadFramework i
     hPut i $ toByteString $ app (fromString wrapper) [quote code]
     hClose i
     eOut <- hGetContents e
     if (eOut == "")
        then (do oOut <- hGetContents o
                 terminateProcess pid
                 return $
                   (case (readExpr oOut) of
                     Left er   -> error $ show er
                     Right cde -> cde))
        else (do terminateProcess pid
                 error eOut)


runPass :: PP b => P423Pass a b -> P423Config -> a -> IO b
runPass p conf code =
    do let code' = pass p conf code
       _res <- runWrapper wn code'
       when (trace p) (printTrace (passName p) code')
       return code'

  where pn = passName p
        wn = wrapperName p
        printTrace :: PP a => String -> a -> IO ()
        printTrace name code = putStrLn ("\n" ++ name ++ ": \n" ++ (unpack $ toByteString $ pp code) ++ "\n")

--------------------------------------------------------------------------------
-- SExp construction helpers

app :: Builder -> [Builder] -> Builder
app rator rands = "(" <> rator <>
                  mconcat (map (" " <>) rands) <>
                  ")"

quote :: PP a => a -> Builder
quote e = app "quote" [ pp e ]

----------------------------------------

instance IsString Builder where
  fromString = BBB.fromString


readUntilBlank :: Handle -> IO ByteString
readUntilBlank hnd = loop []
 where
   loop acc = do
     l <- hGetLine hnd
     case l of
       "" -> return (concat (reverse acc))       
       ll -> loop (ll:acc)

--------------------------------------------------------------------------------
-- Unit tests

-- Evaluate just one expression:
t0 :: IO String
t0 = do SchemeProc{eval,shutdown} <- makeSchemeEvaluator
        a <- eval "(+ 1 2)"
        shutdown
        return$ show a

-- Evaluate multiple expressions:
t1 :: IO (String,String,String)
t1 = do SchemeProc{eval,shutdown} <- makeSchemeEvaluator
        a <- eval "(+ 1 2)"
        b <- eval "(cons '1 '(2 3))"
        c <- eval "(vector 'a 'b 'c)"
        shutdown
        return (show a, show b, show c)

-- This one intentionally creates an error.
t2 :: IO String
t2 = catchit
 where
   catchit = catch io $ \e -> return$ show (e::SomeException)
   io = 
    do SchemeProc{eval,shutdown} <- makeSchemeEvaluator
       a <- eval "(make-vector 1 2 3)"
       shutdown
       return$ show a

-- "Error from child scheme process:\nException: incorrect number of arguments to #<procedure make-vector>\n"

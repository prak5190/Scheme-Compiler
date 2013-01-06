module FrameworkHs.Testing
  ( TestResult(..)
  , runDefault
  , runTests
  , getTests
  , filterInd
  , showResults
  , defaultTestFile
  , defaultConfig
  , RunTests (AllFrom, SelV, SelI)
  , valid, invalid
  ) where

import Control.Exception
import Text.Printf

import FrameworkHs.SExpReader.Parser
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Helpers
import FrameworkHs.Prims
import CompilerHs.Compile

data Tests = Tests { valid :: [LispVal]
                   , invalid :: [LispVal]
                   } deriving (Show)

data TestResult = Pass String | Fail P423Exception
instance Show TestResult where
  show (Pass s) = s
  show (Fail e) = show e

data RunTests
  = AllFrom String
  | SelV [Int] RunTests
  | SelI [Int] RunTests

defaultTestFile :: RunTests
defaultTestFile = AllFrom "test-suite.ss"

defaultConfig :: P423Config
defaultConfig = P423Config
         { framePointerRegister      = RBP
         , allocationPointerRegister = RDX
         , returnAddressRegister     = R15
         , returnValueRegister       = RAX
         , parameterRegisters        = [R8,R9]
         }

catchTestFailures :: P423Exception -> Maybe P423Exception
catchTestFailures e = case e of
  (AssemblyFailedException _)   -> yes
  (ASTParseException _)         -> yes
  (ParseErrorException _)      -> no
  (NoValidTestsException)       ->  no
  (NoInvalidTestsException)     -> no
  (PassFailureException _ _)    -> yes
  (WrapperFailureException _ _) -> yes
  where yes = Just e
        no  = Nothing

-- | Run the default set of tests (all found in the file).
--   Returns the test results for (valid,invalid) tests respectively.
runDefault :: IO ([TestResult],[TestResult])
runDefault = runTests defaultTestFile defaultConfig

showResults :: [TestResult] -> IO ()
showResults = mapM_ print

-- | Returns the test results for (valid,invalid) tests respectively.
runTests :: RunTests -> P423Config -> IO ([TestResult],[TestResult])
runTests tests conf = do
  ts <- getTests tests
  vs <- runSet "Valid" (p423Compile conf) (valid ts)
  is <- runSet "Invalid" (p423Compile conf) (invalid ts)
  testResults vs is
  return (vs,is)

getTests :: RunTests -> IO Tests
getTests t =
  case t of
    AllFrom f  -> lexTests f
    SelV vs t' -> do
      ts <- getTests t'
      let vs' = filterInd vs (valid ts)
      return Tests { valid = vs', invalid = invalid ts }
    SelI is t' -> do
      ts <- getTests t'
      let is' = filterInd is (invalid ts)
      return Tests { valid = valid ts, invalid = is' }

-- | Filter a list to only elements at the given set of positions.
filterInd :: [Int] -> [a] -> [a]
filterInd xs as =
  -- RRN: FIXME: this is quadratic:
  case xs of
    []                              -> []
    (x:xs')
      | x `elem` xs'                -> filterInd xs' as
      | (x >= 0) && (x < length as) -> (as !! x) : filterInd xs' as
      | otherwise                   -> filterInd xs' as

runSet :: String -> (LispVal -> IO String) -> [LispVal] -> IO [TestResult]
runSet name c [] = return []
runSet name c ts =
  do putStrLn ("\nTesting " ++ name)
     putStrLn "Test    Result"
     putStrLn "---------------------------"
     mapIndexed 0 (wrapTest c) ts
  where mapIndexed i f [] = return []
        mapIndexed i f (t:ts) = do a <- f i t
                                   as <- mapIndexed (i+1) f ts
                                   return (a:as)
        wrapTest :: (LispVal -> IO String) -> Int -> LispVal -> IO TestResult
        wrapTest c i l = catchJust catchTestFailures
                           (do res <- c l
                               printf "%4d    Pass\n" i
                               return $ Pass res)
                           (\e ->
                             (do printf "%4d    Fail    %s\n" i (shortExcDescrip e)
                                 return $ Fail e))

testResults :: [TestResult] -> [TestResult] -> IO ()
testResults vs is =
  do putStrLn "\nTesting Summary"
     putStrLn "---------------------------"
     printf "%-24s%4d\n" "Expected Passes:" ep
     printf "%-24s%4d\n" "Unexpected Passes:" up
     printf "%-24s%4d\n" "Expected Failures:" ef
     printf "%-24s%4d\n" "Unexpected Failures:" uf
     printf "%-24s%4d\n\n" "Total:" t
  where ep = countPasses vs
        up = countPasses is
        ef = countFailures is
        uf = countFailures vs
        t  = length vs + length is

countFailures :: [TestResult] -> Int
countFailures = count isFail

countPasses :: [TestResult] -> Int
countPasses = count isPass

isPass :: TestResult -> Bool
isPass (Pass s) = True
isPass (Fail e) = False

isFail :: TestResult -> Bool
isFail (Fail e) = True
isFail (Pass s) = False

count :: (a -> Bool) -> [a] -> Int
count f [] = 0
count f (a:as)
  | f a = 1 + (count f as)
  | otherwise = count f as

lexTests :: String -> IO Tests
lexTests testFile =
  do mls <- lexFile testFile
     case mls of
       Left (Parser pe) -> throw $ ParseErrorException pe
       Right ls -> return $ findTests ls


handleTestFailure :: Int -> P423Exception -> IO TestResult
handleTestFailure i e = do putStrLn (show i ++ " : fail")
                           return $ Fail e

findTests :: [LispVal] -> Tests
findTests l = Tests {valid=findValid l,invalid=findInvalid l}
--findTests [(List l)] = Tests {valid=findValid l,invalid=findInvalid l}

findValid :: [LispVal] -> [LispVal]
findValid [] = throw NoValidTestsException
findValid (t:ts) = case t of
  List (Symbol "valid" : ls) -> ls
  _ -> findValid ts

findInvalid :: [LispVal] -> [LispVal]
findInvalid [] = throw NoInvalidTestsException
findInvalid (t:ts) = case t of
  List (Symbol "invalid" : ls) -> ls
  _ -> findInvalid ts

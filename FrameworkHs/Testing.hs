module FrameworkHs.Testing (testAll, TestResult, testDefault, showResults) where

import Control.Exception
import Text.Printf

import FrameworkHs.SExpReader.Parser
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Helpers
import CompilerHs.Compile

data Tests = Tests { valid :: [LispVal]
                   , invalid :: [LispVal]
                   } deriving (Show)

data TestResult = Pass String | Fail P423Exception
instance Show TestResult where
  show (Pass s) = s
  show (Fail e) = show e

catchTestFailures :: P423Exception -> Maybe P423Exception
catchTestFailures e = case e of
  (AssemblyFailedException) -> yes
  (ASTParseException s) -> yes
  (ParseErrorException pe) -> no
  (NoValidTestsException) ->  no
  (NoInvalidTestsException) -> no
  (PassFailureException p e) -> yes
  (WrapperFailureException w e) -> yes
  where yes = Just e
        no  = Nothing

testDefault :: IO ([TestResult],[TestResult])
testDefault = testAll Default Default

showResults :: [TestResult] -> IO ()
showResults = mapM_ print

testAll :: Option String -> Option P423Config -> IO ([TestResult],[TestResult])
testAll testFile config =
  do ts <- lexTests file
     vs <- runTests "Valid" (p423Compile conf) (valid ts)
     is <- runTests "Invalid" (p423Compile conf) (invalid ts)
     testResults vs is
     return (vs,is)
  where conf = case config of
                 Default  -> defaultConf
                 Option c -> c
        file = case testFile of
                 Default  -> defaultTestFile
                 Option f -> f

runTests :: String -> Compiler -> [LispVal] -> IO [TestResult]
runTests name c ts =
  do putStrLn ("\nTesting " ++ name)
     putStrLn "Test    Result"
     putStrLn "---------------------------"
     mapIndexed 0 (wrapTest c) ts
  where mapIndexed i f [] = return []
        mapIndexed i f (t:ts) = do a <- f i t
                                   as <- mapIndexed (i+1) f ts
                                   return (a:as)
        wrapTest :: Compiler -> Int -> LispVal -> IO TestResult
        wrapTest c i l = catchJust catchTestFailures
                           (do res <- c l
                               printf "%4d    Pass\n" i
                               return $ Pass res)
                           (\e ->
                             (do printf "%4d    Fail    %s\n" i (showShort e)
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

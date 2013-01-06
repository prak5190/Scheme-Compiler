{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,DeriveDataTypeable #-}
module FrameworkHs.Helpers
  (
    -- * Types for compiler configuration and construction
    P423Config ( P423Config
               , framePointerRegister
               , allocationPointerRegister
               , returnAddressRegister
               , returnValueRegister
               , parameterRegisters
               )
  , P423Exception ( AssemblyFailedException
                  , ParseErrorException
                  , ASTParseException
                  , NoValidTestsException
                  , NoInvalidTestsException
                  , PassFailureException
                  , WrapperFailureException
                  )
  , shortExcDescrip
  , P423Pass ( P423Pass
             , pass
             , passName
             , wrapperName
             , trace
             )
  , Option (..)
    
  -- * An alternative `Show` class for printing to X86 assembly code:
  , X86Print, format
  , OpCode

  -- * Emitting text to a handle
  , Out, OutF (Put, Done)
  , output, done, runOut, showOut
  , emitOp1, emitOp2, emitOp3
  , emitLabelLabel
  , emitLabel
  , emitJumpLabel, emitJump
  , emitEntry, emitExit
               
  -- * Shorthands for common emissions:
  , pushq, popq
  , movq, leaq

  -- * Pretty printing:
  , PP, pp, ppSexp

  -- * Parsing
  , parseListWithFinal
  , parseUVar
  , parseFVar
  , parseLabel
  , parseRelop
  , parseBinop
  , parseReg
  , parseInt32
  , parseInt64

  -- * A simple homemade failure type
  , Exc, failure
  , catchExc
    
  -- * Misc numeric and string helpers
  , isInt32
  , isInt64
  , isUInt6
  , wordShift
  , ash
  , chomp
  ) where

import Prelude hiding (LT, EQ, GT)
import Data.List (intercalate)
import Data.Set (size, fromList)
import Data.Char (isDigit, isSpace)
import Data.Int
import Control.Monad (unless)
import qualified Data.Set as S
import Text.Parsec.Error (ParseError)
import Control.Exception
import Data.Typeable
import System.IO

import FrameworkHs.Prims
import FrameworkHs.SExpReader.LispData

data P423Config =
  P423Config
    { framePointerRegister :: Reg
    , allocationPointerRegister :: Reg
    , returnAddressRegister :: Reg
    , returnValueRegister :: Reg
    , parameterRegisters :: [Reg]
    }

data P423Pass a b =
  P423Pass
    { pass :: P423Config -> a -> (Exc b)
    , passName :: String
    , wrapperName :: String
    , trace :: Bool
    }

type Exc = Either String
failure = Left

data Option a = Default | Option a

split :: Char -> String -> (String,String)
split s [] = ([],[])
split s (c:cs)
  | (c == s)  = ([],cs)
  | otherwise = (c:before,cs')
    where (before,cs') = split s cs

------------------------------------------------------------
-- Exceptions ----------------------------------------------

data P423Exception = AssemblyFailedException String
                   | ASTParseException String
                   | ParseErrorException ParseError
                   | NoValidTestsException
                   | NoInvalidTestsException
                   | PassFailureException String String
                   | WrapperFailureException String String
                   deriving (Typeable)

instance Exception P423Exception
instance Show P423Exception where
  show e@(AssemblyFailedException e')   = shortExcDescrip e ++ ": " ++ show e'
  show e@(ParseErrorException e')       = shortExcDescrip e ++ ": " ++ show e'
  show e@(ASTParseException s)          = shortExcDescrip e ++ ": " ++ s
  show e@(NoValidTestsException)        = shortExcDescrip e
  show e@(NoInvalidTestsException)      = shortExcDescrip e
  show e@(PassFailureException p e')       = shortExcDescrip e ++ ": " ++ e'
  show e@(WrapperFailureException w e') = shortExcDescrip e ++ ": " ++ e'

shortExcDescrip :: P423Exception -> String
shortExcDescrip e = case e of
  (AssemblyFailedException e)   -> "Assembly failure"
  (ParseErrorException pe)      -> "SExp parse failure"
  (ASTParseException s)         -> "AST parse failure"
  (NoValidTestsException)       -> "Couldn't find valid tests"
  (NoInvalidTestsException)     -> "Couldn't find invalid tests"
  (PassFailureException p e)    -> "Pass failure (" ++ p ++ ")"
  (WrapperFailureException w e) -> "Wrapper failure (" ++ w ++ ")"


------------------------------------------------------------
-- Emitting ------------------------------------------------

data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Monad (Free f) where
  return = Pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

data OutF b next
  = Put b next
  | Done

type Out = Free (OutF String) ()

instance Functor (OutF b) where
  fmap f (Put x next) = Put x (f next)
  fmap f  Done        = Done

liftF :: (Functor f) => f r -> Free f r
liftF cmd = Free (fmap Pure cmd)

showOut :: (Show a, Show r) => Free (OutF a) r -> String
showOut (Free (Put a x)) =
  "put " ++ show a ++ "\n" ++ showOut x
showOut (Free Done) =
  "done\n"
showOut (Pure r) =
  "return " ++ show r ++ "\n"

output :: b -> Free (OutF b) ()
output x = liftF $ Put x ()
done :: Free (OutF b) ()
done     = liftF Done

runOut :: Free (OutF String) r -> Handle -> IO ()
runOut (Free (Put s x)) h = hPutStrLn h s >> runOut x h
runOut (Free  Done    ) h = return ()
runOut (Pure r)         h = throwIO (userError "runOut: did not call 'done' at the end.")

class X86Print a where
  format :: a -> String

instance X86Print String where
  format = pp

instance X86Print Integer where
  format i = "$" ++ pp i

instance X86Print Reg where
  format r = "%" ++ pp r

instance X86Print Label where
  format (L name ind) = "L" ++ pp ind ++ "(%rip)"

instance X86Print Disp where
  format (D reg off) = pp off ++ "(%" ++ pp reg ++ ")"

instance X86Print Ind where
  format (I bReg iReg) = "(%" ++ pp bReg ++ ", %" ++ pp iReg ++ ")"

type OpCode = String

emitOp1 :: OpCode -> Out
emitOp1 op = output ("    " ++ op)

emitOp2 :: (X86Print a) => OpCode -> a -> Out
emitOp2 op a = output ("    " ++ op ++ " " ++ format a)

emitOp3 :: (X86Print a, X86Print b) => OpCode -> a -> b -> Out
emitOp3 op a b = output ("    " ++ op ++ " " ++ format a ++ ", " ++ format b)

emitLabelLabel :: Label -> Out
emitLabelLabel (L name ind) = output ("L" ++ pp ind ++ ":")

emitLabel :: (X86Print a) => a -> Out
emitLabel a = output (format a ++ ":")

emitJumpLabel :: OpCode -> Label -> Out
emitJumpLabel op (L name ind) = emitOp2 op ("L" ++ pp ind)

emitJump :: (X86Print a) => OpCode -> a -> Out
emitJump op a = emitOp2 op ("*" ++ (format a))

--emitOp1 :: Handle -> OpCode -> IO ()
--emitOp1 h op = hPutStrLn h ("    " ++ op)
--
--emitOp2 :: (X86Print a) => Handle -> OpCode -> a -> IO ()
--emitOp2 h op a = hPutStrLn h ("    " ++ op ++ " " ++ format a)
--
--emitOp3 :: (X86Print a, X86Print b) => Handle -> OpCode -> a -> b -> IO ()
--emitOp3 h op a b = hPutStrLn h ("    " ++ op ++ " " ++ format a ++ ", " ++ format b)
--
--emitLabel :: (X86Print a) => Handle -> a -> IO ()
--emitLabel h a = hPutStrLn h (format a ++ ":")
--
--emitJumpLabel :: Handle -> OpCode -> Label -> IO ()
--emitJumpLabel h op (L name ind) = emitOp2 h op ("L" ++ pp ind)
--
--emitJump :: (X86Print a) => Handle -> OpCode -> a -> IO ()
--emitJump = emitOp2

pushq, popq :: (X86Print a) => a -> Out
movq, leaq :: (X86Print a, X86Print b) => a -> b -> Out
pushq = emitOp2 "pushq"
popq = emitOp2 "popq"
movq = emitOp3 "movq"
leaq = emitOp3 "leaq"

--pushq, popq :: (X86Print a) => Handle -> a -> IO ()
--movq, leaq :: (X86Print a, X86Print b) => Handle -> a -> b -> IO ()
--pushq h = emitOp2 h "pushq"
--popq h = emitOp2 h "popq"
--movq h = emitOp3 h "movq"
--leaq h = emitOp3 h "leaq"

emitEntry :: P423Config -> Out
emitEntry c = do
  emitOp2 ".globl" "_scheme_entry"
  emitLabel "_scheme_entry"
  pushq RBX
  pushq RBP
  pushq R12
  pushq R13
  pushq R14
  pushq R15
  movq  RDI (framePointerRegister c)
  movq  RSI (allocationPointerRegister c)
  leaq  "_scheme_exit(%rip)" (returnAddressRegister c)

--emitEntry :: P423Config -> Handle -> IO ()
--emitEntry c h =
--  do emitOp2 h ".globl" "_scheme_entry"
--     emitLabel h "_scheme_entry"
--     pushq h RBX
--     pushq h RBP
--     pushq h R12
--     pushq h R13
--     pushq h R14
--     pushq h R15
--     movq  h RDI (framePointerRegister c)
--     movq  h RSI (allocationPointerRegister c)
--     leaq  h "_scheme_exit(%rip)" (returnAddressRegister c)

emitExit :: P423Config -> Out
emitExit c = do
  emitLabel "_scheme_exit"
  unless (returnValueRegister c == RAX)
         (movq (returnValueRegister c) RAX)
  popq R15
  popq R14
  popq R13
  popq R12
  popq RBP
  popq RBX
  emitOp1 "ret"

--emitExit :: P423Config -> Handle -> IO ()
--emitExit c h =
--  do emitLabel h "_scheme_exit"
--     unless (returnValueRegister c == RAX)
--            (movq h (returnValueRegister c) RAX)
--     popq h R15
--     popq h R14
--     popq h R13
--     popq h R12
--     popq h RBP
--     popq h RBX
--     emitOp1 h "ret"

------------------------------------------------------------
-- Pretty Printing -----------------------------------------

class PP a where
  pp :: a -> String

ppSexp :: [String] -> String
ppSexp ls = "(" ++ intercalate " " ls ++ ")"

instance PP String where
  pp = id

instance PP Integer where
  pp = show

instance PP UVar where
  pp (UV name ind) = name ++ "." ++ show ind

instance PP FVar where
  pp (FV ind) = "fv" ++ show ind

instance PP Label where
  pp (L name ind) = name ++ "$" ++ show ind

instance PP Disp where
  pp (D r i) = ppSexp ["disp",(pp r),(pp i)]

instance PP Ind where
  pp (I r1 r2) = ppSexp ["index",(pp r1),(pp r2)]

instance PP Relop where
  pp r = case r of
    LT  -> "<"
    LTE -> "<="
    EQ  -> "="
    GTE -> ">="
    GT  -> ">"

instance PP Binop where
  pp b = case b of
    MUL    -> "*"
    ADD    -> "+"
    SUB    -> "-"
    LOGAND -> "logand"
    LOGOR  -> "logor"
    SRA    -> "sra"

instance PP Reg where
  pp r = case r of
    RAX -> "rax"
    RCX -> "rcx"
    RDX -> "rdx"
    RBX -> "rbx"
    RBP -> "rbp"
    RSI -> "rsi"
    RDI -> "rdi"
    R8  -> "r8"
    R9  -> "r9"
    R10 -> "r10"
    R11 -> "r11"
    R12 -> "r12"
    R13 -> "r13"
    R14 -> "r14"
    R15 -> "r15"

------------------------------------------------------------
-- Parsing -------------------------------------------------

parseSuffix :: String -> Exc Integer
parseSuffix i@('0':rest) =
  if (null rest)
     then return 0
     else failure ("Leading zero in index: " ++ i)
parseSuffix i =
  if (and $ map isDigit i)
     then return $ read i
     else failure ("Not a number: " ++ i)

parseListWithFinal :: (LispVal -> Exc a) -> (LispVal -> Exc b) ->
                        [LispVal] -> Exc ([a],b)
parseListWithFinal fa fb [] = failure ("List must have at least one element")
parseListWithFinal fa fb [b] =
  do b <- fb b
     return ([],b)
parseListWithFinal fa fb (a:asb) =
  do a <- fa a
     (as,b) <- parseListWithFinal fa fb asb
     return (a:as,b)

parseUVar :: LispVal -> Exc UVar
parseUVar (Symbol s) = case (split '.' s) of
  (_,"")      -> failure ("No index: " ++ s)
  (name,ind)  -> do ind <- parseSuffix ind; return (UV name ind)
parseUVar e = failure ("Not a symbol: " ++ show e)

parseFVar :: LispVal -> Exc FVar
parseFVar (Symbol s) = case s of
  ('f':'v':ind) -> do ind <- parseSuffix ind; return (FV ind)
  _             -> failure ("Not a framevar: " ++ s)
parseFVar e = failure ("Not a symbol: " ++ show e)

parseLabel :: LispVal -> Exc Label
parseLabel (Symbol s) = case (split '$' s) of
  (_,"")     -> failure ("No index: " ++ s)
  (name,ind) -> do ind <- parseSuffix ind; return (L name ind)
parseLabel e = failure ("Not a symbol: " ++ show e)

-- parseLabel :: LispVal -> Exc Label
-- parseLabel (Symbol s) = case (split '$' s) of
--   (_,"")     -> failure ("No index: " ++ s)
--   (name,ind) -> do ind <- parseSuffix ind; return (L name ind)
-- parseLabel e = failure ("Not a symbol: " ++ show e)

parseRelop :: LispVal -> Exc Relop
parseRelop (Symbol s) = case s of
  "<"  -> return LT
  "<=" -> return LTE
  "="  -> return EQ
  ">=" -> return GTE
  ">"  -> return GT
  e    -> failure ("Not a relop: " ++ e)
parseRelop e = failure ("Not a symbol: " ++ show e)

parseBinop :: LispVal -> Exc Binop
parseBinop (Symbol s) = case s of
  "logand" -> return LOGAND
  "logor"  -> return LOGOR
  "sra"    -> return SRA
  "*"      -> return MUL
  "+"      -> return ADD
  "-"      -> return SUB
  e        -> failure ("Not a binop: " ++ e)
parseBinop e = failure ("Not a symbol: " ++ show e)

parseReg :: LispVal -> Exc Reg
parseReg (Symbol s) = case s of
  "rax" -> return RAX
  "rcx" -> return RCX
  "rdx" -> return RDX
  "rbp" -> return RBP
  "rbx" -> return RBX
  "rsi" -> return RSI
  "rdi" -> return RDI
  "r8"  -> return R8
  "r9"  -> return R9
  "r10" -> return R10
  "r11" -> return R11
  "r12" -> return R12
  "r13" -> return R13
  "r14" -> return R14
  "r15" -> return R15
  e     -> failure ("Not a register: " ++ e)
parseReg e = failure ("Not a symbol: " ++ show e)

parseInt32 :: LispVal -> Exc Integer
parseInt32 (IntNumber i) = if isInt32 n
                              then return n
                              else failure ("Out of range: " ++ show i)
  where n = fromIntegral i
parseInt32 e = failure ("Not an int: " ++ show e)

parseInt64 :: LispVal -> Exc Integer
parseInt64 (IntNumber i) = if isInt64 n
                              then return (fromIntegral n)
                              else failure ("Out of range: " ++ show i)
  where n = fromIntegral i
parseInt64 e = failure ("Not an int: " ++ show e)

------------------------------------------------------------
-- Parse Helpers -------------------------------------------

catchExc :: Exc a -> Exc a -> Exc a
catchExc m1 m2 = case m1 of
  Left e -> m2
  Right a  -> m1

inBitRange :: Integer -> Integer -> Bool
inBitRange r i = (((- (2 ^ (r-1))) <= n) && (n <= ((2 ^ (r-1)) - 1)))
  where n = fromIntegral i

isInt32 = inBitRange 32
isInt64 = inBitRange 64

isUInt6 :: Integer -> Bool
isUInt6 i = (0 <= i) && (i <= 63)

class SuffixTerm a where
  extractSuffix :: a -> Integer
  uniqueSuffixes :: [a] -> Bool
  uniqueSuffixes as = isSet $ map extractSuffix as

isSet :: Ord a => [a] -> Bool
isSet ls = length ls == S.size (S.fromList ls)

instance SuffixTerm UVar where
  extractSuffix (UV name ind) = ind

instance SuffixTerm FVar where
  extractSuffix (FV ind) = ind

instance SuffixTerm Label where
  extractSuffix (L name ind) = ind

wordShift :: Integer
wordShift = 3

ash :: Integer -> Integer -> Integer
ash n = (* (2 ^ n))

-- | Remove whitespace from both ends of a string.
chomp :: String -> String
chomp = reverse . dropWhile isSpace . reverse


module FrameworkHs.Prims
  ( UVar (..)
  , FVar (..)
  , Label (..)
  , Reg (..), numRegisters, allRegisters
  , Relop (..)
  , Binop (..)
  , Disp (..)
  , Ind (..)
  , LooseEq(..)
  )
  where

import Prelude as P hiding (LT, EQ, GT) 
import Data.Int
import Data.Symbol

--------------------------------------------------------------------------------
-- Terminal Type Definitions ---------------------------------------------------

data UVar = UV String Integer deriving (Read, Show, Eq, Ord)
data Label = L String Integer deriving (Read, Show, Eq, Ord)
data FVar = FV Integer        deriving (Read, Show, Eq, Ord)

data UVar' = UV' Symbol Integer deriving (Show, Eq, Ord)
data Label' = L' Symbol Integer deriving (Show, Eq, Ord)

-- | use a loose equality test that only test the suffix of a uvar or label
-- rather than testing the Symbol and the Integer for equality.
class LooseEq a where
  (.=) :: a -> a -> Bool

instance LooseEq UVar where
  (UV var1 suffix1) .= (UV var2 suffix2) = suffix1 P.== suffix2

instance LooseEq Label where
  (L var1 suffix1) .= (L var2 suffix2) = suffix1 P.== suffix2

instance LooseEq UVar' where
  (UV' var1 suffix1) .= (UV' var2 suffix2) = suffix1 P.== suffix2

instance LooseEq Label' where
  (L' var1 suffix1) .= (L' var2 suffix2) = suffix1 P.== suffix2


data Reg = RAX | RCX | RDX | RBX | RBP | RSI | RDI | R8 | R9
         | R10 | R11 | R12 | R13 | R14 | R15
         deriving (Read,Show,Eq,Ord, Bounded, Enum)

allRegisters :: [Reg]
allRegisters = [minBound .. maxBound]

numRegisters :: Int
numRegisters = 1 + fromEnum (maxBound :: Reg) - fromEnum (minBound :: Reg)

data Relop = LT | LTE | EQ | GT | GTE               deriving (Read,Show,Eq,Ord)
data Binop = MUL | ADD | SUB | LOGAND | LOGOR | SRA deriving (Read,Show,Eq,Ord)

data Disp = D Reg Integer deriving (Read,Show,Eq, Ord)
data Ind = I Reg Reg deriving (Read,Show,Eq, Ord)


module FrameworkHs.Prims
  ( UVar (..)
  , FVar (..)
  , Label (..)
  , Reg (..)
  , Relop (..)
  , Binop (..)
  , Disp (..)
  , Ind (..)
  )
  where

import Prelude hiding (LT, EQ, GT)
import Data.Int
import Data.Symbol

--------------------------------------------------------------------------------
-- Terminal Type Definitions ---------------------------------------------------

data UVar = UV String Integer deriving (Read, Show, Eq, Ord)
data FVar = FV Integer        deriving (Read, Show, Eq, Ord)
data Label = L String Integer deriving (Read, Show, Eq, Ord)

data UVar' = UV' Symbol Integer deriving (Show, Eq, Ord)
data Label' = L' Symbol Integer deriving (Show, Eq, Ord)

data Reg = RAX | RCX | RDX | RBX | RBP | RSI | RDI | R8 | R9
         | R10 | R11 | R12 | R13 | R14 | R15
         deriving (Read,Show,Eq,Ord)

data Relop = LT | LTE | EQ | GT | GTE               deriving (Read,Show,Eq,Ord)
data Binop = MUL | ADD | SUB | LOGAND | LOGOR | SRA deriving (Read,Show,Eq,Ord)

data Disp = D Reg Integer deriving (Read,Show,Eq, Ord)
data Ind = I Reg Reg deriving (Read,Show,Eq, Ord)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


module FrameworkHs.GenGrammars.L41FlattenProgram where

import FrameworkHs.Prims
import FrameworkHs.Helpers
import Text.PrettyPrint.HughesPJ (text)
import Blaze.ByteString.Builder (fromByteString)

data Statement
  = Set1 Loc Triv
  | Set2 Loc Binop Triv Triv
  | If1 Relop Triv Triv Label
  | If2 Relop Triv Triv Label
  | Jump Triv
  | LabelS Label
data Triv
  = Loc Loc
  | Integer Integer
  | LabelT Label
data Loc
  = RegL Reg
  | Disp Disp
data Conflict
  = RegC Reg
  | UVar UVar
data Prog
  = Code [Statement] Statement

instance PP Statement where
  pp (Set1 l t) = (ppSexp [fromByteString "set!",(pp l),(pp t)])
  pp (Set2 l b t t2) = (ppSexp [fromByteString "set!",(pp l),(ppSexp [(pp b),(pp t),(pp t2)])])
  pp (If1 r t t2 l) = (ppSexp [fromByteString "if",(ppSexp [(pp r),(pp t),(pp t2)]),(ppSexp [fromByteString "jump",(pp l)])])
  pp (If2 r t t2 l) = (ppSexp [fromByteString "if",(ppSexp [fromByteString "not",(ppSexp [(pp r),(pp t),(pp t2)])]),(ppSexp [fromByteString "jump",(pp l)])])
  pp (Jump t) = (ppSexp [fromByteString "jump",(pp t)])
  pp (LabelS l) = (pp l)
  ppp (Set1 l t) = (pppSexp [text "set!",(ppp l),(ppp t)])
  ppp (Set2 l b t t2) = (pppSexp [text "set!",(ppp l),(pppSexp [(ppp b),(ppp t),(ppp t2)])])
  ppp (If1 r t t2 l) = (pppSexp [text "if",(pppSexp [(ppp r),(ppp t),(ppp t2)]),(pppSexp [text "jump",(ppp l)])])
  ppp (If2 r t t2 l) = (pppSexp [text "if",(pppSexp [text "not",(pppSexp [(ppp r),(ppp t),(ppp t2)])]),(pppSexp [text "jump",(ppp l)])])
  ppp (Jump t) = (pppSexp [text "jump",(ppp t)])
  ppp (LabelS l) = (ppp l)
instance PP Triv where
  pp (Loc l) = (pp l)
  pp (Integer i) = (pp i)
  pp (LabelT l) = (pp l)
  ppp (Loc l) = (ppp l)
  ppp (Integer i) = (ppp i)
  ppp (LabelT l) = (ppp l)
instance PP Loc where
  pp (RegL r) = (pp r)
  pp (Disp d) = (pp d)
  ppp (RegL r) = (ppp r)
  ppp (Disp d) = (ppp d)
instance PP Conflict where
  pp (RegC r) = (pp r)
  pp (UVar u) = (pp u)
  ppp (RegC r) = (ppp r)
  ppp (UVar u) = (ppp u)
instance PP Prog where
  pp (Code l s) = (ppSexp (fromByteString "code" : ((map pp l) ++ [(pp s)])))
  ppp (Code l s) = (pppSexp (text "code" : ((map ppp l) ++ [(ppp s)])))

deriving instance Eq Statement
deriving instance Read Statement
deriving instance Show Statement
deriving instance Ord Statement
deriving instance Eq Triv
deriving instance Read Triv
deriving instance Show Triv
deriving instance Ord Triv
deriving instance Eq Loc
deriving instance Read Loc
deriving instance Show Loc
deriving instance Ord Loc
deriving instance Eq Conflict
deriving instance Read Conflict
deriving instance Show Conflict
deriving instance Ord Conflict
deriving instance Eq Prog
deriving instance Read Prog
deriving instance Show Prog
deriving instance Ord Prog


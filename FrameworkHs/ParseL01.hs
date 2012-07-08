
module FrameworkHs.ParseL01 where

import FrameworkHs.GenGrammars.L01
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers

parseProg :: LispVal -> Exc Prog
parseProg (List ((Symbol "begin"): ls)) =
  do (ss,s) <- parseListWithFinal parseStatement parseStatement ls
     return (Begin ss s)
parseProg e = failure ("Invalid Prog: " ++ show e)

parseStatement :: LispVal -> Exc Statement
parseStatement (List [(Symbol "op-set!"),v1
                     ,List [b,v2,x@(IntNumber _)]]) =
  do v1 <- parseVar v1
     b  <- parseBinop b
     v2 <- parseVar v2
     i <- parseInt32 x
     return (OpSet v1 b v2 i)
parseStatement (List [(Symbol "op-set!"),v1
                     ,List [b,v2,x@(Symbol _)]]) =
  do v1 <- parseVar v1
     b  <- parseBinop b
     v2 <- parseVar v2
     v3 <- parseVar x
     return (OpSet' v1 b v2 v3)
parseStatement (List [(Symbol "set!"),v,x@(IntNumber _)]) =
  do v <- parseVar v
     i <- parseInt64 x
     return (Set v i)
parseStatement (List [(Symbol "set!"),v,x@(Symbol _)]) =
  do v <- parseVar v
     v' <- parseVar x
     return (Set' v v')
parseStatement e = failure ("Invalid Statement: " ++ show e)

parseVar :: LispVal -> Exc Var
parseVar v =
  do v <- parseReg v
     return (Reg v)

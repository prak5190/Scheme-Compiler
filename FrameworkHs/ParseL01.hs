
module FrameworkHs.ParseL01 where

import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers

failure = Left

parseProg :: LispVal -> Exc Prog
parseProg (List ((Symbol "begin"): ls)) =
  do (ss,s) <- parseListWithFinal parseStatement parseStatement ls
     return (Begin ss s)
parseProg e = failure ("Invalid Prog: " ++ show e)

parseStatement :: LispVal -> Exc Statement
parseStatement (List [(Symbol "set!"),v1,rhs]) =
  do v1 <- parseVar v1
     case rhs of
       IntNumber _   -> do i <- parseInt64 rhs
                           return (Set1 v1 i)
       Symbol _      -> do v2 <- parseVar rhs
                           return (Set2 v1 v2)
       List [b,v2,x] -> do b  <- parseBinop b
                           v2 <- parseVar v2
                           case x of
                             IntNumber _ -> do i <- parseInt32 x
                                               return (Set3 v1 b v2 i)
                             Symbol _    -> do v3 <- parseVar x
                                               return (Set4 v1 b v2 v3)
parseStatement e = failure ("Invalid Statement: " ++ show e)

parseVar :: LispVal -> Exc Var
parseVar v =
  do v <- parseReg v
     return (Reg v)

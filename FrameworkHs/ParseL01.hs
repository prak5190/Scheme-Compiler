
module FrameworkHs.ParseL01 where

import Control.Applicative ((<$>))
import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers (parseListWithFinal, parseLabel, parseFVar, parseInt64,
                            parseBinop, parseReg, parseFailureM, PassM, orPassM)

parseProg :: LispVal -> PassM Prog
parseProg (List [(Symbol "letrec"),List bs,t]) =
  do bs <- mapM parseTuple bs
     t <- parseTail t
     return (Letrec bs t)
  where parseTuple :: LispVal -> PassM (Label,Tail)
        parseTuple (List [l,List[(Symbol "lambda"),List [],t]]) =
          do l <- parseLabel l
             t <- parseTail t
             return (l,t)
        parseTuple e = parseFailureM ("Invalid tuple: " ++ show e)
parseProg e = parseFailureM ("Invalid Prog: " ++ show e)

parseTail :: LispVal -> PassM Tail
parseTail (List [t]) =
  do t <- parseTriv t
     return (App t)
parseTail (List ((Symbol "begin"):ls)) =
  do (es,t) <- parseListWithFinal parseEffect parseTail ls
     return (Begin es t)
parseTail e = parseFailureM ("Invalid Tail: " ++ show e)

parseEffect :: LispVal -> PassM Effect
parseEffect (List [(Symbol "set!"),v,List[b,t1,t2]]) =
  do v <- parseVar v
     b <- parseBinop b
     t1 <- parseTriv t1
     t2 <- parseTriv t2
     return (Set2 v b t1 t2)
parseEffect (List [(Symbol "set!"),v,t]) =
  do v <- parseVar v
     t <- parseTriv t
     return (Set1 v t)
parseEffect e = parseFailureM ("Invalid Effect: " ++ show e)

parseTriv :: LispVal -> PassM Triv
parseTriv i@(IntNumber _) =
  do i <- parseInt64 i
     return (Integer i)
parseTriv s@(Symbol _) =
  orPassM (Var   <$> parseVar s)
          (Label <$> parseLabel s)
parseTriv e = parseFailureM ("Invalid Triv: " ++ show e)

parseVar :: LispVal -> PassM Var
parseVar x =
  orPassM (Reg  <$> parseReg x)
          (FVar <$> parseFVar x)

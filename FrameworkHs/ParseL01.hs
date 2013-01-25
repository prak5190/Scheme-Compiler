
module FrameworkHs.ParseL01 where

import Control.Applicative ((<$>))
import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims

import FrameworkHs.Helpers (parseListWithFinal, parseInt32, parseInt64, parseLabel, parseUVar,
                            parseFVar, parseRelop, parseBinop, parseReg, parseFailureM, PassM, orPassM)

parseProg :: LispVal -> PassM Prog
parseProg (List [(Symbol "letrec"),List bs,b]) =
  do
     bs <- mapM parseTuple bs
     b  <- parseBody b
     return (Letrec bs b)
  where parseTuple :: LispVal -> PassM (Label,Body)
        parseTuple (List [l,List[(Symbol "lambda"),List [],b]]) =
          do 
             l <- parseLabel l
             b <- parseBody b
             return (l,b)
        parseTuple e = parseFailureM ("parseProg: Invalid tuple: " ++ show e)
parseProg e = parseFailureM ("parseProg: Invalid Prog: " ++ show e)



parseBody :: LispVal -> PassM Body
parseBody (List [(Symbol "locals"),List bs,t]) =
  do
     us <- mapM parseUVar bs
     t  <- parseTail t
     return (Locals us t)
parseBody e = parseFailureM ("Invalid Body: " ++ show e)


parseTail :: LispVal -> PassM Tail
parseTail (List [(Symbol "if"),p,t1,t2]) =
  do p <- parsePred p
     t1 <- parseTail t1
     t2 <- parseTail t2
     return (IfT p t1 t2)
parseTail (List ((Symbol "begin"):ls)) =
  do (es,t) <- parseListWithFinal parseEffect parseTail ls
     return (BeginT es t)
parseTail (List (t:ls)) =
  do t  <- parseTriv t
     ls <- mapM parseLoc ls
     return (AppT t ls)     
parseTail e = parseFailureM ("Invalid Tail: " ++ show e)


parsePred :: LispVal -> PassM Pred
parsePred (List [(Symbol "true")]) = return (TrueP)
parsePred (List [(Symbol "false")]) = return (FalseP)
parsePred (List ((Symbol "begin"):ls)) =
  do (es,p) <- parseListWithFinal parseEffect parsePred ls
     return (BeginP es p)
parsePred (List [r,t1,t2]) =
  do r  <- parseRelop r
     t1 <- parseTriv t1
     t2 <- parseTriv t2
     return (AppP r t1 t2)
parsePred (List [(Symbol "if"),p1,p2,p3]) =
  do p1 <- parsePred p1
     p2 <- parsePred p2
     p3 <- parsePred p3
     return (IfP p1 p2 p3)
parsePred e = parseFailureM ("Invalid Pred: " ++ show e)


parseEffect :: LispVal -> PassM Effect
parseEffect (List [(Symbol "nop")]) = return (Nop)

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
parseEffect (List [(Symbol "if"),p,e1,e2]) =
  do p <- parsePred p
     e1 <- parseEffect e1
     e2 <- parseEffect e2
     return (IfE p e1 e2)
parseEffect (List ((Symbol "begin"):ls)) =
  do (es,e) <- parseListWithFinal parseEffect parseEffect ls
     return (BeginE es e)
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
  orPassM (UVar <$> parseUVar x)
          (Loc  <$> parseLoc  x)

parseLoc :: LispVal -> PassM Loc
parseLoc x =
  orPassM (Reg  <$> parseReg x)
          (FVar <$> parseFVar x)


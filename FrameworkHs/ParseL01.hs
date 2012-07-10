
module FrameworkHs.ParseL01 where

import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers

parseProg :: LispVal -> Exc Prog
parseProg (List [(Symbol "letrec"),List bs,t]) =
  do bs <- mapM parseTuple bs
     t <- parseTail t
     return (Letrec bs t)
  where parseTuple :: LispVal -> Exc (Label,Tail)
        parseTuple (List [l,List[(Symbol "lambda"),List [],t]]) =
          do l <- parseLabel l
             t <- parseTail t
             return (l,t)
        parseTuple e = failure ("Invalid tuple: " ++ show e)
parseProg e = failure ("Invalid Prog: " ++ show e)

parseTail :: LispVal -> Exc Tail
parseTail (List [(Symbol "app"), t]) =
  do t <- parseTriv t
     return (App t)
parseTail (List ((Symbol "begin"):ls)) =
  do (es,t) <- parseListWithFinal parseEffect parseTail ls
     return (Begin es t)
parseTail e = failure ("Invalid Tail: " ++ show e)

parseEffect :: LispVal -> Exc Effect
parseEffect (List [(Symbol "op-set!"),v,List[b,t1,t2]]) =
  do v <- parseVar v
     b <- parseBinop b
     t1 <- parseTriv t1
     t2 <- parseTriv t2
     return (OpSet v b t1 t2)
parseEffect (List [(Symbol "set!"),v,t]) =
  do v <- parseVar v
     t <- parseTriv t
     return (Set v t)
parseEffect e = failure ("Invalid Effect: " ++ show e)

parseTriv :: LispVal -> Exc Triv
parseTriv i@(IntNumber _) =
  do i <- parseInt64 i
     return (Integer i)
parseTriv s@(Symbol _) =
  catchExc (do v <- parseVar s
               return (Var v))
           (do l <- parseLabel s
               return (Label l))
parseTriv e = failure ("Invalid Triv: " ++ show e)

parseVar :: LispVal -> Exc Var
parseVar x =
  catchExc (do r <- parseReg x
               return (Reg r))
           (do f <- parseFVar x
               return (FVar f))

-- (l-01
--   (start Prog)
--   (Prog
--    (letrec ((Label (lambda () Tail)) *) Tail))
--   (Tail
--    (app Triv)
--    (begin Effect * Tail))
--   (Effect
--    (set! Var Triv)
--    (set! Var (Binop Triv Triv)))
--   (Triv
--    Var
--    Integer
--    Label)
--   (Var
--    Reg
--    FVar))

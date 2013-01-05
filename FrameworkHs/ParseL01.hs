
module FrameworkHs.ParseL01 where

import FrameworkHs.GenGrammars.L01VerifyScheme
import FrameworkHs.SExpReader.LispData
import FrameworkHs.Prims
import FrameworkHs.Helpers

parseProg :: LispVal -> Exc Prog
parseProg (List [(Symbol "letrec"),List bs,b]) =
  do bs <- mapM parseTuple bs
     b <- parseBody b
     return (Letrec bs b)
  where parseTuple :: LispVal -> Exc (Label,Body)
        parseTuple (List [l,List[(Symbol "lambda"),List [],b]]) =
          do l <- parseLabel l
             b <- parseBody b
             return (l,b)
        parseTuple e = failure ("Invalid tuple: " ++ show e)
parseProg e = failure ("Invalid Prog: " ++ show e)

parseBody :: LispVal -> Exc Body
parseBody (List [(Symbol "locate"),List bs,t]) =
  do bs <- mapM parseTuple bs
     t <- parseTail t
     return (Locate bs t)
  where parseTuple :: LispVal -> Exc (UVar,Loc)
        parseTuple (List [u,l]) =
          do u <- parseUVar u
             l <- parseLoc l
             return (u,l)
        parseTuple e = failure ("Invalid tuple: " ++ show e)
parseBody e = failure ("Invalid Body: " ++ show e)

parseTail :: LispVal -> Exc Tail
parseTail (List [t]) =
  do t <- parseTriv t
     return (AppT t)
parseTail (List [(Symbol "if"),p,t1,t2]) =
  do p <- parsePred p
     t1 <- parseTail t1
     t2 <- parseTail t2
     return (IfT p t1 t2)
parseTail (List ((Symbol "begin"):ls)) =
  do (es,t) <- parseListWithFinal parseEffect parseTail ls
     return (BeginT es t)
parseTail e = failure ("Invalid Tail: " ++ show e)

parsePred :: LispVal -> Exc Pred
parsePred (List [(Symbol "true")]) = return (TrueP)
parsePred (List [(Symbol "false")]) = return (FalseP)
parsePred (List ((Symbol "begin"):ls)) =
  do (es,p) <- parseListWithFinal parseEffect parsePred ls
     return (BeginP es p)
parsePred (List [r,t1,t2]) =
  do r <- parseRelop r
     t1 <- parseTriv t1
     t2 <- parseTriv t2
     return (AppP r t1 t2)
parsePred (List [(Symbol "if"),p1,p2,p3]) =
  do p1 <- parsePred p1
     p2 <- parsePred p2
     p3 <- parsePred p3
     return (IfP p1 p2 p3)
parsePred e = failure ("Invalid Pred: " ++ show e)

parseEffect :: LispVal -> Exc Effect
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
  catchExc (do u <- parseUVar x
               return (UVar u))
           (do l <- parseLoc x
               return (Loc l))

parseLoc :: LispVal -> Exc Loc
parseLoc x =
  catchExc (do r <- parseReg x
               return (Reg r))
           (do f <- parseFVar x
               return (FVar f))

-- (l-01
--   (start Prog)
--   (Prog
--    (letrec ((Label (lambda () Body)) *) Body))
--   (Body
--    (locate ((UVar Loc) *) Tail))
--   (Tail
--    (Triv)
--    (if Pred Tail Tail)
--    (begin Effect * Tail))
--   (Pred
--    (true)
--    (false)
--    (Relop Triv Triv)
--    (if Pred Pred Pred)
--    (begin Effect * Pred))
--   (Effect
--    (nop)
--    (set! Var Triv)
--    (op-set! Var (Binop Triv Triv))
--    (if Pred Effect Effect)
--    (begin Effect * Effect))
--   (Triv
--    Var
--    Int
--    Label)
--   (Var
--    UVar
--    Loc)
--   (Loc
--    Reg
--    FVar))

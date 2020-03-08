module Ruse.Eval where

import Ruse.Syntax

--
--
-- Big-step evaluator
--
-- Given an RExpr, evaluate it to either Right of an RExpr representing the
-- final value, or Left of a String representing a runtime error. Use the error
-- strings that have been provided; you shouldn't need any other runtime errors.
--
-- You should not try to detect if the original RExpr represents a
-- non-terminating program---your evaluator should simply loop in that case.
--
-- When evaluating an RExpr with multiple components (for instance, `Plus`), you
-- should try to evaluate the components in order. If any component returns an
-- error (Left _), your evaluator should return this error (rather than changing
-- the error). Note, however, that some constructs are lazy (And, Or, If, Cond).
-- The big-step semantics document describes how these things should be
-- evaluated.
--
--
eval :: RExpr -> Either String RExpr
eval e = case e of
           Plus e1 e2 -> evalPlus e1 e2
           Subt e1 e2 -> evalSubt e1 e2
           Mult e1 e2 -> evalMult e1 e2
           Ifte e e1 e2 -> evalIfte e e1 e2
           And e1 e2 -> evalAnd e1 e2
           Or e1 e2 -> evalOr e1 e2
           Not e -> evalNot e
           IsEq e1 e2 -> evalIsEq e1 e2
           IsLt e1 e2 -> evalIsLt e1 e2
           IsGt e1 e2 -> evalIsGt e1 e2
           IsNil e -> evalIsNil e
           List e -> evalList e
           Cons e1 e2 -> evalCons e1 e2
           Car e -> evalCar e
           Cdr e -> evalCdr e
           App e1 e2 -> evalApp e1 e2
           Rec e -> evalRec e
           NumC e -> Right (NumC e)
           BoolC e -> Right (BoolC e)
           StrC e -> Right (StrC e)
           Var e -> Right (Var e)
           Lam e -> Right (Lam e)
           _ -> Left "Default message"

-- We've done Plus for you, but the code is very ugly. Start by cleaning up this
-- function using do-notation. You should be able to eliminate the first two
-- case analyses.
{-evalPlus :: RExpr -> RExpr -> Either String RExpr
evalPlus e1 e2 =
  case eval e1 of
    Left s1 -> Left s1
    Right e1' ->
      case eval e2 of
        Left s2 -> Left s2
        Right e2' ->
          case (e1', e2') of
            (NumC i1, NumC i2) -> return $ NumC $ i1 + i2
            _ -> Left "Add on non-numeric"
-}

evalPlus :: RExpr -> RExpr -> Either String RExpr
evalPlus e1 e2 = do (eval e1) >>= (\first ->
                      (eval e2) >>= (\second ->
                        case (first, second) of
                          (NumC i1, NumC i2) -> return $ NumC $ i1 + i2
                          _ -> Left "Add on non-numeric"))


evalSubt :: RExpr -> RExpr -> Either String RExpr
evalSubt e1 e2 = do (eval e1) >>= (\first ->
                      (eval e2) >>= (\second ->
                        case (first, second) of
                          (NumC i1, NumC i2) -> return $ NumC $ i1 - i2
                          _ -> Left "Sub on non-numeric"))


evalMult :: RExpr -> RExpr -> Either String RExpr
evalMult e1 e2 = do (eval e1) >>= (\first ->
                      (eval e2) >>= (\second ->
                        case (first, second) of
                          (NumC i1, NumC i2) -> return $ NumC $ i1 * i2
                          _ -> Left "Mul on non-numeric"))


evalIfte :: RExpr -> RExpr -> RExpr -> Either String RExpr
evalIfte e e1 e2 = do (eval e) >>= (\condition ->
                            case condition of
                                  (BoolC True) -> (eval e1) >>= (\first ->
                                              eval first)
                                  (BoolC False) -> (eval e2) >>= (\second ->
                                              eval second)
                                  _ -> Left "If-then-else guard not Boolean")

evalAnd :: RExpr -> RExpr -> Either String RExpr
evalAnd e1 e2 = do (eval e1) >>= (\first ->
                      case first of 
                        BoolC False -> return $ BoolC False
                        BoolC True -> (eval e2) >>= (\second ->
                          case second of 
                            BoolC False -> return $ BoolC False
                            BoolC True -> return $ BoolC True
                            _ -> Left "And on non-Boolean")
                        _ -> Left "And on non-Boolean")

evalOr :: RExpr -> RExpr -> Either String RExpr
evalOr e1 e2 = do (eval e1) >>= (\first ->
                      case first of 
                        BoolC True -> return $ BoolC True
                        BoolC False -> (eval e2) >>= (\second ->
                          case second of 
                            BoolC True -> return $ BoolC True
                            BoolC False -> return $ BoolC False
                            _ -> Left "Or on non-Boolean")
                        _ -> Left "Or on non-Boolean")

evalNot :: RExpr -> Either String RExpr
evalNot e = do (eval e) >>= (\first ->
                      case first of 
                        BoolC True -> return $ BoolC False
                        BoolC False -> return $ BoolC True
                        _ -> Left "Not on non-Boolean")

evalIsEq :: RExpr -> RExpr -> Either String RExpr
evalIsEq e1 e2 = do (eval e1) >>= (\first ->
                      (eval e2) >>= (\second ->
                        case (first, second) of
                          (NumC i1, NumC i2) -> (if i1==i2 then (return $ BoolC True) else (return $ BoolC False))
                          _ -> Left "Equality on non-numeric"))

evalIsLt :: RExpr -> RExpr -> Either String RExpr
evalIsLt e1 e2 = do (eval e1) >>= (\first ->
                      (eval e2) >>= (\second ->
                        case (first, second) of
                          (NumC i1, NumC i2) -> (if i1<i2 then (return $ BoolC True) else (return $ BoolC False))
                          _ -> Left "Lt on non-numeric"))

evalIsGt :: RExpr -> RExpr -> Either String RExpr
evalIsGt e1 e2 = do (eval e1) >>= (\first ->
                      (eval e2) >>= (\second ->
                        case (first, second) of
                          (NumC i1, NumC i2) -> (if i1>i2 then (return $ BoolC True) else (return $ BoolC False))
                          _ -> Left "Gt on non-numeric"))

evalIsNil :: RExpr -> Either String RExpr
evalIsNil e = do (eval e) >>= (\first ->
                      case first of 
                        List [] -> return $ BoolC True
                        List (x:xs) -> return $ BoolC False
                        _ -> Left "IsNil on non-list")

evalList :: [RExpr] -> Either String RExpr
evalList es = do case es of
                  [] -> return $ List [] 
                  (x:xs) -> do  first <- (eval x)
                                rest <- (mapM eval xs)
                                return $ List (first:rest)

evalCons :: RExpr -> RExpr -> Either String RExpr
evalCons e e' = do  v <- (eval e)
                    (eval e') >>= (\first -> 
                      case first of
                        List xs -> return $ List (v : xs)
                        _ -> Left "Cons on non-list")

evalCar :: RExpr -> Either String RExpr
evalCar e = do case e of
                List [] -> Left "Cons on non-list"
                List (x:xs) -> return x
-- Other error: Left "Car of non-list"

evalCdr :: RExpr -> Either String RExpr
evalCdr e = do case e of
                List [] -> Left "Cdr of empty list"
                List (x:xs) -> return $ List xs
-- Other error: Left "Cdr of non-list"

evalApp :: RExpr -> RExpr -> Either String RExpr
evalApp e e' = do v <- (eval e')
                  (eval e) >>= (\first ->
                    case first of
                      Lam f -> (eval (subst v f))
                      _ -> Left "Apply non-function")

evalRec :: RExpr -> Either String RExpr
evalRec e = do (eval e) >>= (\first -> eval (subst first first))
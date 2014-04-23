import Prelude hiding (lookup)
import Data.Map (Map, lookup, fromList)
import Data.Maybe (fromMaybe)

data Formula = Var Char
             | Not Formula
             | Formula :/\: Formula
             | Formula :\/: Formula
             | Formula :->: Formula
             | Formula :<->: Formula
             deriving (Eq)
                      
instance Show Formula where
    show (Var c) = show c
    show (Not c) = '¬' : show c
    show (e1 :\/: e2) = show' e1 "∨" e2
    show (e1 :/\: e2) = show' e1 "∧" e2
    show (e1 :->: e2) = show' e1 "→" e2
    show (e1 :<->: e2) = show' e1 "↔" e2

type Mapping = Map Char Bool

-- Given a formula and a map, evaluates the truth value.
-- Example:
--     > eval ((Var 'j') :\/: (Var 'k')) (fromList [('j', True), ('k', False)])
--       True
eval :: Formula -> Mapping -> Bool
eval (Var v) vs = fromMaybe False (lookup v vs)
eval (Not wff) vs = not $ eval wff vs
eval (e1 :\/: e2) vs = eval e1 vs || eval e2 vs
eval (e1 :/\: e2) vs = eval e1 vs && eval e2 vs
eval (e1 :->: e2) vs = not (eval e1 vs) || eval e2 vs
eval (e1 :<->: e2) vs = eval e1 vs == eval e2 vs

-- Reduces a formula to conjunctive normal form.
cnf :: Formula -> Formula
cnf f = f
  
prove :: Formula -> Bool
prove _ = False

show' :: Formula -> String -> Formula -> String
show' e1 s e2 = (show e1) ++ " " ++ s ++ " " ++ (show e2)

{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (lookup)
import Data.Map (Map, lookup, fromList)
import Data.Maybe (fromMaybe)
import Data.List (nub, permutations)
import Data.List.Split (splitEvery)

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

-- Given a formula, return a list of all the variables.
-- TODO Fix this to remove duplicates.

variables :: Formula -> [Char]
variables f = nub $ variables' f

variables' :: Formula -> [Char]
variables' (Var c) = [c]
variables' (Not e) = variables' e
variables' (e1 :\/: e2) = variables'' e1 e2
variables' (e1 :/\: e2) = variables'' e1 e2
variables' (e1 :->: e2) = variables'' e1 e2
variables' (e1 :<->: e2) = variables'' e1 e2

variables'' e1 e2 = variables' e1 ++ variables' e2

-- Generate all possible assignments of truth values to variable.
-- (p.s. this function is a disgusting mess)
-- Either this is too slow or formulas need to be simplified before being fed to it.
assignments :: Formula -> [Mapping]
assignments f = nub $ map fromList $ splitEvery (length vs) $ perms vs
  where
    vs = variables f
    half tf vz = zip (concat (permutations vz)) (cycle tf)
    perms vz = half [True, False] vz ++ half [False, True] vz

tautology :: Formula -> Bool
tautology f = and $ map (eval f) (assignments f)

unsatisfiable :: Formula -> Bool
unsatisfiable f = tautology $ Not f

satisfiable :: Formula -> Bool
satisfiable f = not $ unsatisfiable f

-- Reduces a formula to negation normal form.
nnf :: Formula -> Formula
nnf (e1 :/\: e2) = (nnf e1) :/\: (nnf e2)
nnf (e1 :\/: e2) = (nnf e1) :\/: (nnf e2)
nnf (e1 :->: e2) = (nnf (Not e1)) :\/: (nnf e2)
nnf (e1 :<->: e2) = ((nnf e1) :/\: (nnf e2)) :\/: ((nnf (Not e1)) :/\: (nnf (Not e2)))
nnf (Not (Not e1)) = nnf e1
nnf (Not (e1 :/\: e2)) = (nnf (Not e1)) :\/: (nnf (Not e2))
nnf (Not (e1 :\/: e2)) = (nnf (Not e1)) :/\: (nnf (Not e2))
nnf (Not (e1 :->: e2)) = (nnf e1) :/\: (nnf (Not e2))
nnf (Not (e1 :<->: e2)) = ((nnf e1) :/\: (nnf (Not e2))) :\/: ((nnf (Not e1)) :/\: (nnf e2))
nnf f = f

-- Reduces a formula to conjunctive normal form.
cnf :: Formula -> Formula
cnf f = f
  
prove :: Formula -> Bool
prove _ = False

show' :: Formula -> String -> Formula -> String
show' e1 s e2 = (show e1) ++ " " ++ s ++ " " ++ (show e2)

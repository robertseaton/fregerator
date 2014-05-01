{-# OPTIONS_GHC -Wall #-}

module Prover
       ( Formula (..)
       , Mapping
       , cnf
       , dnf
       , equivalent
       , eval
       , nnf
       , satisfiable
       , show
       , tautology
       , unsatisfiable
       , variables
       )
       where

import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Functor ((<$>))
import Data.Map (Map, lookup, fromList)
import Data.Maybe (fromMaybe)
import Data.List (nub, permutations)
import Data.List.Split (splitEvery)
import Test.QuickCheck

type Mapping = Map Char Bool

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

instance Arbitrary Formula where
  arbitrary = randomFormula


randomFormula :: Gen Formula
randomFormula = sized randomFormula'

randomFormula' :: Int -> Gen Formula
randomFormula' n | n > 0 = oneof [ randomVar
                                  , randomNot boundedFormula
                                  , randomBin boundedFormula
                                  ]
              | otherwise = randomVar
  where
    boundedFormula = randomFormula' (n `div` 4)

randomNot :: Gen Formula -> Gen Formula
randomNot e = Not <$> e

randomBin :: Gen Formula -> Gen Formula
randomBin e = oneof . map (\c -> liftM2 c e e)
               $ [(:/\:), (:\/:), (:->:), (:<->:)]

randomVar :: Gen Formula
randomVar = Var <$> arbitrary

show' :: Formula -> String -> Formula -> String
show' e1 s e2 = (show e1) ++ " " ++ s ++ " " ++ (show e2)

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
-- (p.s. this function is a disgusting mess and exp time)
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
-- A formula is in negation normal form when NOT is only applied to variables,
-- and is otherwise a seqence of ANDs and ORs.
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

-- Reduces a formula to disjunctive normal form.
-- A formula is in disjunction normal form when it's a disjunction of conjunctions
-- or literals.
-- For example: (a :/\: b) :\/: (b :/\: c)
dnf :: Formula -> Formula
dnf = dnf' . nnf
  where
    dnf' (e1 :/\: e2) = (dnf' e1) `dist` (dnf' e2)
    dnf' (e1 :\/: e2) = (dnf' e1) :\/: (dnf' e2)
    dnf' expr = expr

    -- Distributive law, propositional logic.
    dist (e1 :\/: e2) e3 = (e1 `dist` e3) :\/: (e2 `dist` e3)
    dist e1 (e2 :\/: e3) = (e1 `dist` e2) :\/: (e1 `dist` e3)
    dist e1 e2 = e1 :/\: e2

-- Reduces a formula to conjunctive normal form.
-- A formula is in conjunctive normal form when it's a conjunction of disjunctions.
cnf :: Formula -> Formula
cnf = cnf' . nnf
  where
    cnf' (e1 :/\: e2) = (cnf' e1) :/\: (cnf' e2)
    cnf' (e1 :\/: e2) = (cnf' e1) `dist` (cnf' e2)
    cnf' expr = expr

    dist (e1 :/\: e2) e3 = (e1 `dist` e3) :/\: (e2 `dist` e3)
    dist e1 (e2 :/\: e3) = (e1 `dist` e2) :/\: (e1 `dist` e3)
    dist e1 e2 = e1 :\/: e2
    

-- TODO: This doesn't handle the empty list.
concatf :: [Formula] -> (Formula -> Formula -> Formula) -> Formula
concatf [f] _ = f
concatf (x:xs) op = x `op` (concatf xs op)

conj :: [Formula] -> Formula
conj f = concatf f (:/\:)

disj :: [Formula] -> Formula
disj f = concatf f (:\/:)

iffj :: [Formula] -> Formula
iffj f = concatf f (:<->:)

equivalent' :: [Formula] -> Bool
equivalent' f = tautology $ iffj f

equivalent :: Formula -> Formula -> Bool
equivalent f1 f2 = equivalent' [f1, f2]

prove :: Formula -> Bool
prove _ = False

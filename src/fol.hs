import Data.List(intersperse)
-- A term is either a function applied to a list of variables, or a variable.
-- So, the equivalent of Constr String [Term] in more habitual notation is f(a, b, c)
-- while the equivalent of Var string is 
data Term = Const String [Term]
            | Var String

data FOL = TT
         | FF
         | Atom String [Term]
         | Not FOL
         | And FOL FOL
         | Or FOL FOL
         | Impl FOL FOL
         | Exists String FOL
         | Forall String FOL

instance Show Term where
  show (Const f args) = id f ++ showt args
  show (Var c) = id c

instance Show FOL where
  show (TT) = "True"
  show (FF) = "False"
  show (Atom f args) = id f ++ showt args
  show (Not c) = '¬' : show c
  show (And e1 e2) = show' e1 "∧" e2
  show (Or e1 e2) = show' e1 "∨" e2
  show (Impl e1 e2) = show' e1 "→" e2
  show (Exists c f) = "∃." ++ show f
  show (Forall c f) = "∀." ++ show f

showt :: [Term] -> String
showt a = parenw $ concat $ intersperse ", " $ map show a

-- Wraps a string in parentheses.
parenw :: String -> String
parenw s = "(" ++ s ++ ")"

show' :: FOL -> String -> FOL -> String
show' e1 op e2 = show e1 ++ " " ++ op ++ " " ++ show e2

simplify :: FOL -> FOL
simplify = undefined

-- Reduce a sentence in first-order logic to negation normal form.
nnf :: FOL -> FOL
nnf (And p q) = And (nnf p) (nnf q)
nnf (Or p q) = Or (nnf p) (nnf q)
nnf (Impl p q) = Or (nnf (Not p)) (nnf q)
nnf (Forall x p) = Forall x (nnf p)
nnf (Exists x p) = Exists x (nnf (Not p))
nnf (Not (Forall x p)) = Exists x (nnf (Not p))
nnf (Not (Exists x p)) = Forall x (nnf (Not p))
nnf f = f

pnf :: FOL -> FOL
pnf = undefined

cnf :: FOL -> FOL
cnf = undefined

resolution :: FOL -> Bool
resolution = undefined

import Data.List(intersperse)
-- A term is either a function applied to a list of variables, or a variable.
-- So, the equivalent of Constr String [Term] in more habitual notation is f(a, b, c)
-- while the equivalent of Var string is 
data Term = Const String [Term]
            | Var String

data FOL = Impl FOL FOL
         | Atom String [Term]
         | Not FOL
         | TT
         | FF
         | Or FOL FOL
         | And FOL FOL
         | Exists String FOL
         | Forall String FOL

instance Show Term where
  show (Const f args) = id f ++ "(" ++ showt args ++ ")"
  show (Var c) = id c

showt :: [Term] -> String
showt a = concat $ intersperse ", " $ map show a

instance Show FOL where
  show (Not c) = '¬' : show c
  show (Or e1 e2) = show' e1 "∨" e2
  show (And e1 e2) = show' e1 "∧" e2
  show (Impl e1 e2) = show' e1 "→" e2
  show (TT) = "True"
  show (FF) = "False"
  show (Exists c f) = "∃." ++ show f
  show (Forall c f) = "∀." ++ show f
  show (Atom f args) = id f ++ "(" ++ showt args ++ ")"


show' :: FOL -> String -> FOL -> String
show' e1 op e2 = show e1 ++ " " ++ op ++ " " ++ show e2

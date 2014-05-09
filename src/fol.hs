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

  


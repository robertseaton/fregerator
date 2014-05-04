data Term = Const String [Term]
            | Var String

data FOL = Impl FOL FOL
         | Atom String [Term]
         | Not FOL
         | TT
         | FF
         | Or FOL FOL
         | And FOL FOL
         | Exists (Term -> FOL)
         | Forall (Term -> FOL)

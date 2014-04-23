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
    show (e1 :\/: e2) = (show e1) ++ " ∧ " ++ (show e2)
    show (e1 :/\: e2) = (show e1) ++ " ∨ " ++ (show e2)
    show (e1 :->: e2) = (show e1) ++ " → " ++ (show e2)
    show (e1 :<->: e2) = (show e1) ++ " ↔ " ++ (show e2)

cnf :: Formula -> Formula
cnf _ = ((Var 'a') :\/: (Var 'b'))
  
prove:: Formula -> Bool
prove _ = False

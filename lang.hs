module Lang where

import Data.List

type I = String
type Name = String

data Prog = Prog [Def] Expr
data Def = FDef Name [([Pattern], Expr)] | CDef Name Expr
data Expr =
  Symbol I |
  Var I |
  App Expr Expr |
  Lambda [Pattern] Expr | -- single case
  Where [Def] Expr
data Pattern = PatS I [Pattern] | PatV I

data T = S I [T]
       | V I
       | A T T
       | L [(P, T)] -- single argument
       | E String   -- error
       | F          -- pattern match failure 

data P = PS I [P] | PV I

-- eval returns S, L, or E (or diverges)
eval :: T -> T
eval (S s ts) = S s ts
eval (L cs) = L cs
eval (A (S s ts) t2) = S s (ts ++ [t2])
eval (A (L []) _) = E "a l [] _"
eval (A (L ((p,te):cs)) t2)= case apply p t2 te of
  F -> eval (A (L cs) t2)
  x -> eval x
eval (A (E e) _) = E e
eval (A (V v) _) = E "a v _"
eval (A a t2) = eval (A (eval a) t2)
eval (V v) = E "a v"
eval F = E "a f"
eval (E e) = E e
    
-- apply a simple lambda to an argument, returns S, L, E, or F
apply :: P -> T -> T -> T
apply (PV v)    ta te = replace v ta te
apply (PS s ps) ta te = case eval ta of
  S s' ts | s == s' -> multiapply ps ts te
          | s /= s' -> F
  L _ -> F
  E e -> E e

multiapply :: [P] -> [T] -> T -> T
multiapply [] [] te = te
multiapply [] ts te = F
multiapply ps [] te = F
multiapply (p:ps) (t:ts) te = case apply p t te of
  F -> F
  E e -> E e
  te' -> multiapply ps ts te'

replace :: String -> T -> T -> T
replace v ta (V v') | v == v' = ta
                    | otherwise = (V v')
replace v ta (S s ts) = S s (map (replace v ta) ts)
replace v ta (L cs) = L cs' where
  cs' = map f cs
  f (p, te) = if inPat v p then (p,te) else (p, replace v ta te)
replace v ta (A t1 t2) = (A (replace v ta t1) (replace v ta t2))
replace _ _ (E e) = error "replacing in an ERROR?"
replace _ _ F = error "replacing in an FAIL?"

inPat :: String -> P -> Bool
inPat v (PS _ ps) = or (map (inPat v) ps)
inPat v (PV v') = v == v'

instance Show T where
  show (S s []) = s
  show (S s ts) = "(" ++ s ++ " " ++ concat (intersperse " " (map show ts)) ++ ")"
  show (L []) = "(λ.)"
  show (L [(p,t)]) = "(λ" ++ show p ++ ". " ++ show t ++ ")"
  show (L cs) = "(mλ" ++ concat (intersperse " " (map (\(p,t) -> "(λ" ++ show p ++ ". " ++ show t ++ ")") cs)) ++ ")"
  show (A t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (V v) = v
  show (E e) = "ERROR: " ++ e
  show F = "NO_MATCH"

instance Show P where
  show (PS s []) = s
  show (PS s ps) = "(" ++ s ++ " " ++ concat (intersperse " " (map show ps)) ++ ")"
  show (PV v) = v

-- using this for products for now
pI :: I
pI = "Π"

y :: T
y = let h = L [(PV "x", A (V "f") (A (V "x") (V "x")))] in L [(PV "f", A h h)]

succ2 :: T
succ2 = L
  [ (PS "One" [], A (S "O" []) (S "One" [])),
    (PS "O" [PV "b"], A (S "I" []) (V "b")),
    (PS "I" [PV "b"], A (S "O" []) (A (V "succ") (V "b"))) ]

succ1 :: T
succ1 = L [(PV "succ", succ2)]

succ :: T
succ = (A y succ1)

one = S "One" []
two = A (S "O" []) one
three = A (S "I" []) one
four = S "O" [S "O" [one]]

full :: T -> T
full (S s []) = S s []
full (S s ts) = S s (map (full . eval) ts)
full (L cs) = L cs

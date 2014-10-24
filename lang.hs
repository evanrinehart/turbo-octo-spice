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
       | E          -- error
       | F          -- pattern match failure 

data P = PS I [P] | PV I

-- eval returns S, L, or E (or diverges)
eval :: T -> T
eval (S s ts) = S s ts
eval (L cs) = L cs
eval (A (S s ts) t2) = S s (ts ++ [t2])
eval (A (L []) _) = E
eval (A (L ((p,te):cs)) t2)= case apply p t2 te of
  F -> eval (A (L cs) t2)
  x -> x
eval (A E _) = E
eval (A (V v) _) = E
eval (A a t2) = eval (A (eval a) t2)
eval (V v) = E
eval F = E
eval E = E
    
-- apply a simple lambda to an argument, returns S, L, E, or F
apply :: P -> T -> T -> T
apply (PV v)    ta te = eval (replace v ta te)
apply (PS s ps) ta te = case eval ta of
  S s' ts | s == s' -> multiapply ps ts te
          | s /= s' -> F
  L _ -> F
  E -> E

multiapply :: [P] -> [T] -> T -> T
multiapply [] [] te = te
multiapply [] ts te = F
multiapply ps [] te = F
multiapply (p:ps) (t:ts) te = case apply p t te of
  F -> F
  E -> E
  te' -> multiapply ps ts te'

replace :: String -> T -> T -> T
replace v ta (V v') | v == v' = ta
                    | otherwise = (V v')
replace v ta (S s ts) = S s (map (replace v ta) ts)
replace v ta (L cs) = L cs' where
  cs' = map f cs
  f (p, te) = if inPat v p then (p,te) else (p, replace v ta te)
replace v ta (A t1 t2) = (A (replace v ta t1) (replace v ta t2))
replace _ _ E = error "replacing in an ERROR?"
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
  show E = "ERROR"
  show F = "NO_MATCH"

instance Show P where
  show (PS s []) = show s
  show (PS s ps) = "(" ++ show s ++ " " ++ concat (intersperse " " (map show ps)) ++ ")"
  show (PV v) = v

-- using this for products for now
pI :: I
pI = "Π"

    

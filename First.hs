module Initial where

import Data.Char
import Control.Applicative
import Control.Monad.State

data Nat = Zero | Succ Nat

val :: Nat -> Int
val Zero = 0
val (Succ x) = 1 + val x

nats = iterate Succ Zero
upshift = tail $ map Var nats
downshift = Var Zero:map Var nats

data Exp = Var Nat | App Exp Exp | Lam Exp

subst :: [Exp] -- expressions to replace each free variables
      -> Exp   -- haystack
      -> Exp   -- new haystack
subst sigma (Var n) = sigma !! val n
subst sigma (App e1 e2) = App (subst sigma e1) (subst sigma e2)
subst sigma (Lam e1) = Lam $ subst (Var Zero:map (subst upshift) sigma) e1

beta :: Exp -> Exp
beta (App (Lam e1) e2) = let s = subst upshift e2 : (map Var $ tail nats)
                         in subst downshift $ subst s e1
beta (App e1 e2) = App (beta e1) (beta e2)
beta x = x

names = map pure ['a'..'z'] ++ ['x':(show x) | x <- [(0::Integer)..]]

type NameState = State [String]

pretty, pretty' :: [String] -> Exp -> NameState String
pretty' g e@(App _ _) = parens <$> pretty g e
        where parens a = "(" ++ a ++ ")"
pretty' g e = pretty g e
pretty g (Var n) = pure $ g !! val n
pretty g (App e1 e2) = space <$> pretty g e1 <*> pretty' g e2
       where space a b = a ++ " " ++ b
pretty g (Lam e) = do
  (x:xs) <- get
  put xs
  body <- pretty (x:g) e
  return $ "(λ" ++ x ++ ". " ++ body ++ ")"

pprint :: Exp -> String
pprint e = fst $ runState (pretty (map (map toUpper) names) e) names

main = let y = Lam (App (Lam (App (Var (Succ Zero)) (App (Var Zero) (Var Zero)))) (Lam (App (Var (Succ Zero)) (App (Var Zero) (Var Zero))))) in putStrLn $ pprint $ iterate beta (App y (Var Zero)) !! 500

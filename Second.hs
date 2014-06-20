{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
module Second where

import Control.Applicative
import Control.Monad.State

data Exp a = Var a | App (Exp a) (Exp a) | Lam (Exp (Maybe a))
     deriving Functor

instance Monad Exp where
  return = Var
  (>>=) :: Exp a -> (a -> Exp b) -> Exp b
  (Var e) >>= sigma = sigma e
  (App e1 e2) >>= sigma = App (e1 >>= sigma) (e2 >>= sigma)
  (Lam e) >>= sigma = let sigma' Nothing = return Nothing
                          sigma' (Just x) = fmap Just $ sigma x
                      in Lam (e >>= sigma')

instance Applicative Exp where
  pure = return
  (<*>) = ap

beta :: Exp a -> Exp a
beta (App (Lam e1) e2) = e1 >>= maybe e2 return
beta (App e1 e2) = App (beta e1) (beta e2)
beta x = x

names = map pure ['a'..'z'] ++ ['x':(show x) | x <- [(0::Integer)..]]

type NameState = State [String]

pretty, pretty' :: (a -> String) -> Exp a -> NameState String
pretty' g e@(App _ _) = parens <$> pretty g e
        where parens a = "(" ++ a ++ ")"
pretty' g e = pretty g e
pretty g (Var n) = pure $ g n
pretty g (App e1 e2) = space <$> pretty g e1 <*> pretty' g e2
       where space a b = a ++ " " ++ b
pretty g (Lam e) = do
  (x:xs) <- get
  put xs
  body <- pretty (maybe x g) e
  return $ "(λ" ++ x ++ ". " ++ body ++ ")"

pprint :: Show a => Exp a -> String
pprint e = fst $ runState (pretty show e) names

main = let y = Lam (App (Lam (App (Var (Just Nothing)) (App (Var Nothing) (Var Nothing)))) (Lam (App (Var (Just Nothing)) (App (Var Nothing) (Var Nothing)))))
       in putStrLn $ pprint $ iterate beta (App y (Var ())) !! 500

module Example where

data Expr = T
          | F
          | Hash
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr Expr

eval :: Expr -> Maybe Bool
eval T = Just True
eval F = Just False
eval Hash = Nothing
eval (Not e) = fmap not (eval e)
eval (And l r) = case eval l of
                  Just False -> Just False
                  Just True -> eval r
                  Nothing -> case eval r of
                    Just False -> Just False
                    _ -> Nothing
eval (Or l r) = case eval l of
                  Just True -> Just True
                  Just False -> eval r
                  Nothing -> case eval r of
                    Just True -> Just True
                    _ -> Nothing
eval (If c t e) = do
                    c' <- eval c
                    if c' then eval t else eval e

contract :: Expr -> Expr -> Expr -> Expr
contract a b c = If (Or a b) (And a c) b

ex1 :: Expr
ex1 = If (Or T Hash) (And T T) Hash

ex2 :: Expr
ex2 = If (Or Hash T) (And Hash F) Hash

ex3 :: Expr
ex3 = If (Or F F) Hash F

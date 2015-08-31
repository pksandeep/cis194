{-# LANGUAGE TypeSynonymInstances #-}

module Calc where
import ExprT
import Parser
import StackVM (StackExp,Program)

eval :: ExprT -> Integer
eval e = case e of
           Lit n -> n
           Add e1 e2 -> eval(e1) + eval(e2)
           Mul e1 e2 -> eval(e1) * eval(e2)

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Just expr -> Just(eval expr)
  otherwise -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add a b = Add a b
  mul a b = Mul a b

instance Expr Integer where
  lit = id
  add a b = a+b
  mul a b = a*b

instance Expr Bool where
  lit n =  n > 0
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 (a+b)
  mul (Mod7 a) (Mod7 b) = Mod7 (a*b)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Program where
  lit n = error
  add a b = error
  mul a b = error

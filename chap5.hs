{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Kind (Constraint, Type)

five :: Int
five = 5

five_ :: (a ~ Int) => a
five_ = 5

data Expr a where
  LitInt  :: (a ~ Int) => Int -> Expr a
  LitBool :: Bool             -> Expr Bool
  Add     :: Expr Int         -> Expr Int -> Expr Int
  Not     :: Expr Bool        -> Expr Bool
  If      :: Expr Bool        -> Expr a   -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt i)  = i
evalExpr (LitBool b) = b
evalExpr (Add x y)   = evalExpr x + evalExpr y
evalExpr (Not x)     = not $ evalExpr x
evalExpr (If b x y)  = if evalExpr b
                         then evalExpr x
                         else evalExpr y

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t


showBool :: HList '[_1 , Bool, _2] -> String
showBool (_ :# b :# _2 :# HNil) = show b

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

instance Show (HList '[]) where
  show HNil = ""

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (x :# xs) = show x ++ ", " ++ show xs

instance Ord (HList '[]) where
  HNil <= HNil = True

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  (a :# as) <= (b :# bs) = a <= b && as <= bs

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[]       = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
   All c '[]       = ()
   All c (t ': ts) = (c t, All c ts)

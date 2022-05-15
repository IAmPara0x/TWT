{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

import Prelude hiding (fst)
import Data.Kind (Type)


class Eval l t | l -> t where
  eval :: l -> t


newtype Fst a b = Fst (a,b) deriving (Show)

instance Eval (Fst a b) a where
  eval (Fst (a,b)) = a

instance Eval [a] (Maybe a) where

  eval [a] = Just a
  eval _   = Nothing

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList f [])     = []
  eval (MapList f (a:as)) = eval (f a) : eval (MapList f as)

type Exp a = a -> Type

type family TEval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance TEval (Snd '(a,b)) = b

data FromMaybe :: a -> Maybe a -> Exp a

type instance TEval (FromMaybe _1 ('Just a)) = a
type instance TEval (FromMaybe a 'Nothing)   = a

data ListToMaybe :: [a] -> Exp (Maybe a)

type instance TEval (ListToMaybe  '[])          = 'Nothing
type instance TEval (ListToMaybe (a ': '[]))    = 'Nothing
type instance TEval (ListToMaybe (a ': '[as]))  = 'Just a

data TMapList :: (a -> Exp b) -> [a] -> Exp [b]

type instance TEval (TMapList f '[]) = '[]
type instance TEval (TMapList f (a ': as)) = TEval a ': TEval (TMapList f as)

data TFoldr :: (a -> b -> b) -> b -> [a] -> Exp b

type instance TEval (TFoldr f b '[]) = b
type instance TEval (TFoldr f b (a ': as)) = TEval (TFoldr f (f a b) as)


data (|>>=) :: Exp a -> (a -> Exp b) -> Exp b

type instance TEval (a |>>= f) = TEval (f (TEval a))

data TyEq :: a -> b -> Exp Bool

type instance TEval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance TEval (Map f 'Nothing)   = 'Nothing
type instance TEval (Map f ('Just a))  = 'Just (TEval (f a))

type instance TEval (Map f '(a,b)) = '(TEval (f a), b)


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Functor.Identity
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf hiding (Any (..), type (+))
import Fcf.Data.List
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

type Any :: (k -> Type) -> Type
data Any f where
  Any :: Show (f t) => f t -> Any f

type OpenProduct :: (Type -> Type) -> [(Symbol, Type)] -> Type
data OpenProduct f ts where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

type Key :: Symbol -> Type
data Key key = Key

type Dict = OpenProduct Identity

type UniqueKey (key :: k) (ts :: [(k, t)]) =
  Null =<< Filter (TyEq key <=< Fst) ts

type LookupType (key :: s) (ts :: [(s, t)]) = (LookupTypeImpl key ts)

data LookupTypeImpl :: s -> [(s, t)] -> Exp t

type instance Eval (LookupTypeImpl k ts) = Eval (FromMaybe Stuck =<< Lookup k ts)

type FindElem (key :: k) (ts :: [(k, t)]) = FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts

type UpdateElem (key :: k) (t :: m) (ts :: [(k, m)]) =
  SetIndex (Eval (FindElem key ts)) '(key, t) ts

data MemberImpl :: k -> [(k, t)] -> Exp Constraint

type instance Eval (MemberImpl k ts) = (KnownNat (Eval (FindElem k ts)))

type Member (key :: k) (ts :: [(k, t)]) = Eval (MemberImpl key ts)

type RemoveKey (key :: k) (ts :: [(k, t)]) = Filter (Not <=< TyEq key <=< Fst) ts

type UpsertElem
  (key :: Symbol)
  (t :: k)
  (ts :: [(Symbol, k)]) =
  FromMaybe ('(key, t) ': ts)
    =<< Map (Placeholder10f3 SetIndex '(key, t) ts)
    =<< FindIndex (TyEq key <=< Fst) ts

data Placeholder10f3 :: (a -> b -> c -> Exp r) -> b -> c -> a -> Exp r

type instance Eval (Placeholder10f3 f b c a) = Eval (f a b c)

type UpsertLoc (key :: Symbol) (ts :: [(Symbol, k)]) = Eval (FindIndex (TyEq key <=< Fst) ts)

class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

instance KnownNat n => FindUpsertElem ('Just n) where
  upsertElem = Just . fromIntegral . natVal $ Proxy @n

instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

findElem :: forall t ts. Member t ts => Int
findElem = fromInteger . natVal $ Proxy @(Eval (FindElem t ts))

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

insert' ::
  forall key f t ts.
  (Show (f t), Eval (UniqueKey key ts) ~ True) =>
  Key key ->
  f t ->
  OpenProduct f ts ->
  OpenProduct f ('(key, t) ': ts)
insert' _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

get' ::
  forall key f ts.
  Member key ts =>
  Key key ->
  OpenProduct f ts ->
  f (Eval (LookupType key ts))
get' _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

update' ::
  forall key f t ts.
  (Member key ts, Show (f t)) =>
  Key key ->
  f t ->
  OpenProduct f ts ->
  OpenProduct f (Eval (UpdateElem key t ts))
update' _ ft (OpenProduct v) = OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

delete' ::
  forall key f ts.
  (Member key ts) =>
  Key key ->
  OpenProduct f ts ->
  OpenProduct f (Eval (RemoveKey key ts))
delete' _ (OpenProduct v) =
  OpenProduct $
    V.foldMap (\(i, a) -> if i == idx then mempty else V.singleton a) $
      V.zip (V.fromList [0 .. (V.length v)]) v
  where
    idx = findElem @key @ts

upsert' ::
  forall key f t ts.
  (FindUpsertElem (UpsertLoc key ts), Show (f t)) =>
  Key key ->
  f t ->
  OpenProduct f ts ->
  OpenProduct f (Eval (UpsertElem key t ts))
upsert' _ ft (OpenProduct v) = OpenProduct $
  case upsertElem @(UpsertLoc key ts) of
    Nothing -> V.cons (Any ft) v
    Just n -> v V.// [(n, Any ft)]

insert ::
  forall key t ts.
  (Show (Identity t), Eval (UniqueKey key ts) ~ True) =>
  t ->
  OpenProduct Identity ts ->
  OpenProduct Identity ('(key, t) ': ts)
insert t = insert' @key @Identity @t Key (Identity t)

get ::
  forall key ts.
  Member key ts =>
  OpenProduct Identity ts ->
  Eval (LookupType key ts)
get = runIdentity . get' @key @Identity Key

update ::
  forall key t ts.
  (Member key ts, Show (Identity t)) =>
  t ->
  OpenProduct Identity ts ->
  OpenProduct Identity (Eval (UpdateElem key t ts))
update t = update' @key @Identity Key (Identity t)

upsert ::
  forall key t ts.
  (FindUpsertElem (UpsertLoc key ts), Show (Identity t)) =>
  t ->
  OpenProduct Identity ts ->
  OpenProduct Identity (Eval (UpsertElem key t ts))
upsert t = upsert' @key @Identity Key (Identity t)

delete ::
  forall key ts.
  (Member key ts) =>
  OpenProduct Identity ts ->
  OpenProduct Identity (Eval (RemoveKey key ts))
delete = delete' @key Key

------------------------------------------------------

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

instance Show (HList '[]) where
  show HNil = " ]"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (x :# HNil) = "[ " ++ show x ++ show HNil
  show (x :# xs) = "[ " ++ show x ++ ", " ++ drop 2 (show xs)

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

type GetResults
  (keys :: [Symbol])
  (f :: Type -> Type)
  (ts :: [(Symbol, Type)]) =
  Map (Pure1 f) =<< Map (Flip LookupTypeImpl ts) keys

type Keys :: [Symbol] -> Type
data Keys keys = Keys

class Gets (keys :: [Symbol]) (ts :: [(Symbol, Type)]) where
  gets' :: Keys keys -> OpenProduct f ts -> HList (Eval (GetResults keys f ts))

instance Gets '[] ts where
  gets' _ _ = HNil

instance (Member x ts, Gets xs ts) => Gets (x ': xs) ts where
  gets' _ p = get' @x Key p :# gets' @xs Keys p

type Apply (f :: Type -> Type) (xs :: [Type]) = Eval (Map (Pure1 f) xs)

type InsertsType (keys :: [Symbol]) (xs :: [Type]) (ts :: [(Symbol, Type)]) =
  Eval (Eval (Zip keys xs) ++ ts)

class Inserts (keys :: [Symbol]) (f :: (Type -> Type)) (xs :: [Type]) (ts :: [(Symbol, Type)]) where
  inserts' ::
    Keys keys ->
    HList (Apply f xs) ->
    OpenProduct f ts ->
    OpenProduct f (InsertsType keys xs ts)

instance Inserts '[]  f '[] ts where
  inserts' _ _ p = p

instance
  ( Show (f x),
    Eval (UniqueKey k (InsertsType ks xs ts)) ~ True,
    Inserts ks f xs ts
  ) =>
  Inserts (k ': ks) f (x ': xs) ts where
  inserts' _ (ft :# fts) p = insert' @k @f @x Key ft (inserts' @ks @f @xs Keys fts p)

gets ::
  forall keys ts.
  (Gets keys ts) =>
  OpenProduct Identity ts ->
  HList (Eval (GetResults keys Identity ts))
gets = gets' @keys @_ @Identity Keys

inserts ::
  forall keys xs ts.
  (Inserts keys Identity xs ts) =>
  HList (Apply Identity xs) ->
  OpenProduct Identity ts ->
  OpenProduct Identity (InsertsType keys xs ts)

inserts = inserts' @keys @Identity @xs @ts Keys
  

x = insert @"yuno" @String "Yuno Gasai" nil

y = insert @"ints" @[Int] [1 .. 10] x

z = insert @"bool" True $ insert @"rem" @String "Rem" y


main :: IO ()
main = print (gets @'["yuno", "rem"] z)

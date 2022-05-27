{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce
import Fcf hiding (type (+))

import Data.Functor.Identity


data OpenSum (f :: k -> Type) (ts :: [k]) :: Type where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type Dict = OpenSum Identity

type FindElem (key :: k) (ts :: [k]) = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromInteger . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
      then Just (unsafeCoerce f)
      else Nothing

weaken :: forall f x ts. OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum idx ft) = UnsafeOpenSum (idx + 1) ft

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ unsafeCoerce (n - 1) t


insert :: forall t ts. Member t ts => Identity t -> Dict ts
insert = inj @Identity

get :: forall t ts. Member t ts => Dict ts -> Maybe (Identity t)
get = prj @Identity



y :: Dict '[Bool, Int]
y = weaken @Identity @Bool $ insert @Int @'[Int] (Identity 10)


x :: Peano 2
x = Succ $ Succ Zero

main :: IO()
main = print "Yuno"

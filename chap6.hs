{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

import Data.Function ((&))

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

f :: Int -> (forall a. a -> a)
f _ = id

{-
 forall a b. (a -> b) -> (forall c. c -> a)

 forall r. (forall a. a -> r) -> r
-}


cont :: forall a. a -> (forall r. (a -> r) -> r)
cont a = (a &)

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = f id

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
  fmap ab (Cont f) = Cont (f ab &)

instance Applicative Cont where
  pure a                = Cont (a &)
  (Cont f) <*> (Cont g) = Cont ((f id $ g id) &)

instance Monad Cont where
  return         = pure
  (Cont a) >>= f = f $ a id

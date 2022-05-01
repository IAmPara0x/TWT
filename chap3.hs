{-# LANGUAGE DataKinds #-}

import GHC.TypeLits

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 g) = T1 (f . g)


-- | @T2 is not a @Functor, it's a @Contravariant Functor.
newtype T2 a = T2 (a -> Int)


-- | @T3 is not a @Functor.
newtype T3 a = T3 (a -> a)

-- | @T4 is not a @Functor because there's no way to 'access' @a in the @fmap.
newtype T4 a = T4 ((Int -> a) -> Int)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap ab (T5 aii) = T5 $ \bi -> aii (bi . ab)


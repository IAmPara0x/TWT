{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

newtype ST s a = ST
  { unsafeRunST :: a
  }

instance Functor (ST s) where
  fmap :: (a -> b) -> ST s a -> ST s b
  fmap f = ST . f . unsafeRunST

instance Applicative (ST s) where
  pure :: a -> ST s a
  pure = ST

  (<*>) :: ST s (a -> b) -> ST s a -> ST s b
  (ST f) <*> (ST a) = ST (f a)

instance Monad (ST s) where
  return :: a -> ST s a
  return = pure

  (>>=) :: ST s a -> (a -> ST s b) -> ST s b
  (ST a) >>= f = f a


newtype STRef s a = STRef
  { unSTRef :: IORef a
  }

newSTRef :: forall a s. a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

runST :: (forall s. ST s a) -> a
runST = unsafeRunST

safeExample :: forall s. ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref

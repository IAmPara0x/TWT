{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Data
import Data.Foldable (asum)
import Data.Maybe    (fromMaybe)
import Data.Kind

data Any = forall a. Show a => Any a

data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  show = elimHasShow show

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

elimAny :: (forall a. Show a => a -> String) -> Any -> String
elimAny f (Any a) = f a

data Dynamic where
  Dynamic :: (Typeable a) => a -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a
fromDynamic  = elimDynamic cast

liftD2 :: forall a b r. ( Typeable a
                        , Typeable b
                        , Typeable r
                        )
                     => Dynamic
                     -> Dynamic
                     -> (a -> b -> r)
                     -> Maybe Dynamic
liftD2 d1 d2 f =
    fmap Dynamic . f
    <$> fromDynamic d1
    <*> fromDynamic d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = 
  fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int @Int a b (+)
    ]

data Has (c :: Type -> Constraint) :: Type where
  Has :: c t => t -> Has c

type NewHasShow = Has Show
type NewDynamic = Has Typeable

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

isMemty :: (MonoidEq a) => a -> Bool
isMemty a = a == mempty


class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a

newtype ST s a =
  ST { unsafeRunST :: a
     }

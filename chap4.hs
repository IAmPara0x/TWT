{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

import GHC.TypeLits

broken :: (a -> b) -> a -> b
broken f a = error "doesn't work."
  where
    apply = f a

working :: forall a b. (a -> b) -> a -> b
working f a = apply
  where
    apply :: b
    apply = f a

fmapM :: forall a b. (a -> b) -> Maybe a -> Maybe b
fmapM = fmap @Maybe @_ @_

type family AlwaysUnit (a :: *) :: * where
  AlwaysUnit a = ()

type family Or (x :: Bool) (y :: Bool) :: * where
  Or 'True y = Maybe Bool
  Or _     y = Bool


true :: Or 'True 'True -> Bool
true (Just _)    = True
true Nothing     = True


false :: Or 'False 'False -> Bool
false _ = False

alwaysNone :: forall a. AlwaysUnit a -> Maybe a
alwaysNone () = Nothing @a

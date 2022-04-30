{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

import Prelude hiding (or, not)
import GHC.TypeLits


data UserType = Basic
              | Admin
              deriving (Show)


data User :: UserType -> * where
  Normal    :: String -> User Basic
  Previlged :: String -> User Admin

auth :: User Admin -> IO()
auth (Previlged key) = print key

admin :: User Admin
admin = Previlged "admin password"

basic :: User Basic
basic = Normal "normal password"

or :: Bool -> Bool -> Bool
or True _ = True
or _    y = y

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or _     y = y


{-
  Ex 2.4-i: Write a closed type family to compute Not.
-}

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True


{-

  We can't use Map because, type family must be saturated,
  therefore we can't get a type family of type (a -> a)
  and use it in the Map.

-}

type family Map (x :: a -> a) (i :: [a]) :: [a] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs


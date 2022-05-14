{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


import           Prelude hiding (Either(..))
import           Data.Coerce   (Coercible(..), coerce)
import           Data.Foldable (toList)
import           Data.Monoid   (Sum(..), Product(..))
import           GHC.TypeLits

import qualified Data.Map as M

type role Either representational representational
data Either a b = Left a | Right b
                  deriving (Show, Eq)

type role Proxy phantom
data Proxy a = Proxy

x :: Either Int Int
x = Left 1

y :: Either (Sum Int) (Product Int)
y = coerce x

type family IntToBool (a :: *) :: * where
  IntToBool Int = Bool
  IntToBool a   = a

type Names = [Symbol]

name :: Symbol -> String
name = undefined

main :: IO()
main = print "Yuno"

type Theme = [Symbol]

class HasColor (color :: Symbol) (theme :: Theme)

instance HasColor color (color : rest)
instance {-# Overlappable #-}
  HasColor color rest => HasColor color (color' : rest)

testFunction :: HasColor color theme => ()
testFunction = ()

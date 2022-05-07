{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}


import           Prelude hiding (Either(..))
import           Data.Coerce   (Coercible(..), coerce)
import           Data.Foldable (toList)
import           Data.Monoid   (Sum(..), Product(..))

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

main :: IO()
main = print "Yuno"

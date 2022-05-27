{-# LANGUAGE DeriveGeneric #-}

import qualified GHC.Generics as GHC
import Generics.SOP


data A   = C Bool | D A Int | E (B ())
          deriving (Show, GHC.Generic)

instance Generic A

data B a = F | G a Char Bool
          deriving (Show, GHC.Generic)

instance Generic (B a)

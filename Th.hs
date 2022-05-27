{-# LANGUAGE TemplateHaskellQuotes #-}

module Th where

import Control.Monad
import Language.Haskell.TH

curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map (Just . VarE) xs)
  return $ LamE args (AppE (VarE f) ntup)

getCurryN :: Int -> DecsQ
getCurryN n = return <$> funD (mkName $ "curry" ++ show n) [clause [] (normalB $ curryN n) []]

addn :: Integer -> DecQ
addn n = do
  x <- newName "x"
  let name = mkName $ "add" <> show n
      exp  = LamE [VarP x] $ AppE (AppE (VarE '(+)) (LitE (IntegerL n))) (VarE x)
  
  return $ FunD name [ Clause [] (NormalB exp) []]

addsN :: Integer -> DecsQ
addsN n = forM [1..n] addn
  
  
main :: IO()
main = print "Yuno"

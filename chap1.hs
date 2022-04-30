{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use curry" #-}
{-# HLINT ignore "Use uncurry" #-}

data X = A | B
        deriving (Show)

f :: Maybe X -> X
f (Just A) = A
f (Just B) = B
f Nothing  = A


data Level     = Top | Mid | Bottom
               deriving (Show)

data Direction = DLeft | DCenter | DRight
               deriving (Show)

newtype TicTacToe a = TicTacToe { board :: Level -> Direction -> a }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = TicTacToe $ const $ const Nothing

{-
  Ex 1.4-i: Use Curry–Howard to prove the exponent law that
            a^b × a^c = a^(b+c) .
-}

ex1_4_i :: (b -> a , c -> a) -> (Either b c -> a)
ex1_4_i (f, g) = \case
                    (Left b)  -> f b
                    (Right c) -> g c

ex1_4_i' :: (Either b c -> a) -> (b -> a , c -> a)
ex1_4_i' f = (f . Left, f. Right)


{-
Ex 1.4-ii: Prove (a × b)^c = a^c × b^c.
-}

ex1_4_ii :: (c -> (a,b)) -> (c -> a, c -> b)
ex1_4_ii f = (fst . f, snd . f)

ex1_4_ii' :: (c -> a, c -> b) -> (c -> (a,b))
ex1_4_ii' (f,g) = \c -> (f c, g c)


{-
  Ex 1.4-iii: Prove (a^b)^c = a^(b×c)
-}

ex1_4_iii :: (c -> (b -> a)) -> ((c,b) -> a)
ex1_4_iii f = \(c,b) -> f c b

ex1_4_iii' :: ((c,b) -> a) -> (c -> (b -> a))
ex1_4_iii' f = \c b -> f (c,b)

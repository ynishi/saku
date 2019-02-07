module Lib
    ( someFunc,
    Eating(..),
    Lib.sum
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Price = Integer
type Name = String
type Category = String
data Eating = Food Category Name Price | Eating [Eating] deriving (Show, Eq, Ord)

sum :: Eating -> Integer
sum (Food _ _ p) = p
sum (Eating xs) = foldl (\z food -> z + Lib.sum(food)) 0 xs

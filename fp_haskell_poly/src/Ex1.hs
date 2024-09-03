module Ex1 where

data Fraction  = Fraction Int Int deriving Show

instance Eq Fraction where 
    (==) (Fraction n d) (Fraction n' d') = undefined -- fixme

instance Ord Fraction where 
    compare (Fraction n d) (Fraction n' d') = undefined -- fixme 



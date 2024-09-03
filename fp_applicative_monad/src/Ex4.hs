module Ex4 where 


data Mk s a = Mk {runMk:: s -> Maybe (a, s)} 


instance Functor (Mk s) where 
    -- fmap :: (a -> b) -> Mk s a -> Mk s b 
    fmap = undefined -- fix me

instance Applicative (Mk s) where 
    -- pure :: a -> Mk s a 
    pure = undefined -- fix me
    -- (<*>) :: Mk s (a -> b) -> Mk s a -> Mk s b
    (<*>) = undefined -- fix me

instance Monad (Mk s) where 
    -- (>>=) :: Mk s a -> (a -> Mk s b) -> Mk s b
    (>>=) = undefined -- fix me

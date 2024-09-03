module Ex2 where 

data PP a b = PP a b 
    deriving (Show, Eq) 

instance Functor (PP c) where 
    -- fmap :: (a -> b) -> PP c a -> PP c b
    fmap = undefined -- fixme 

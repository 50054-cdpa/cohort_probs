module Ex3 where 

data BST a = Empty
    | Node a (BST a) (BST a) 
    deriving Show 


instance Eq a => Eq (BST a) where 
    (==) = undefined -- fix me

insert :: Ord a => a -- ^ the key to be inserted
    -> BST a -- ^ the input BST
    -> BST a -- ^ the result BST 
insert = undefined -- fixme 

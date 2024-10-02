% Parametric Polymorphism and Adhoc Polymorphism 


# Learning Outcome

By this end of this lesson, you should be able to 

* develop parametrically polymorphic Haskell code using Generic, Algebraic Datatype
* safely mix parametric polymoprhism with adhoc polymoprhism (overloading) using type classes 
* develop generic programming style code using `Functor` type class.
* make use of `Option` and `Either` to handle and manipulate errors and exceptions. 

# Exercise 1

Consider the following built-in type classes 

```hs
class Eq a where 
    (==) :: a -> a -> Bool

data Ordering = LT | EQ | GT

class Eq a => Ord a where 
    compare :: a -> a -> Ordering
```

The `Eq a =>` appearing in the `Ord a` type class definition declares that `Ord a` is a derived class of `Eq a`,
i.e. every instance of `Ord a` must be implementing `Eq a`. 
We can define a type class instance for ordering among Boolean and integers (although they have been provided in the prelude).

```hs
instance Eq Bool where 
    (==) True True   = True 
    (==) False Fales = True
    (==) _ _         = False

instance Ord Bool where 
    compare True  True  = EQ
    compare False False = EQ 
    compare True  False = GT
    compare False True  = LT
```

Define the `Eq` and `Ord` instances for the following data type 

```hs
data Fraction = Fraction Int Int 
    deriving Show
```
> The `deriving Show` clause makes a `Fraction` "printable". 


# Exercise 2

Define a polymoprhic quicksort function which makes use of `Ord` type class context.


```hs
quicksort :: Ord a => [a] -> [a]
```

# Exercise 3

Given the following algebraic data type of a polymorphic binary search tree, and a type class `Ord`. 

```hs
data BST a = Empty | Node a (BST a) (BST a) deriving 
```

Complete the following `insert` for `BST` which makes use of the `Ord a` type class context. Note that duplicate key will be dropped. 

```hs
insert :: Ord a => a -- ^ the key to be inserted
    -> BST a  -- ^ the input BST
    -> BST a  -- ^ the result BST
```


# Exercise 4

Define a `subtree` function which searches for the sub-tree rooted at the given key in a BST.


```hs
subtree :: Ord a => a 
    -> BST a 
    -> Maybe (BST a)
```

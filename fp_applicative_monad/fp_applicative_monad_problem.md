# 50.054 - Applicative and Monad


## Learning Outcomes



1. Describe and define derived type class
2. Describe and define Applicative Functors
3. Describe and define Monads
4. Apply Monad to in design and develop highly modular and resusable software.


## Exercise 1 

Given the `Functor Maybe` definition as follows, 

```hs
instance Functor Maybe where 
    fmap f Nothing  = Nothing 
    fmap f (Just x) = Just (f x)
```

Verify that it satisfies the Functor Laws.



## Exercise 2

Given the following type lambda of a pair type.
```hs
data PP a b = PP a b 
    deriving (Show, Eq) 
```

Define the type instance of `Functor (PP c)`.


## Exercise 3

Given the following definition of `Applicative Maybe`

```hs
instance Applicative Maybe where 
    pure x = Just x 
    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing 
    (<*>) (Just f) (Just x) = Just (f x)
```

Show that the above instance satisfies the applicative laws.

## Exercise 4

Consider the following data type, complete the implementation of `Functor (Mk s)`, `Applicative (Mk s)` and `Monad (Mk s)`.

```hs
data Mk s a = Mk {runMk:: s -> Maybe (a, s)} 
```


## Exercise 5


Consider the following code


```hs
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
data Env = Env { indent :: Int}


indentEnv :: Env -> Env
indentEnv (Env i) = Env (i + 4)

spaces :: Int -> String
spaces n = replicate n ' '

rmPrint :: (Show a) => BTree a -> Reader Env String
rmPrint = undefined 

tree = Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty, Empty)) Empty
```
Define a function `rmPrint` which uses a reader monad to print the tree `tree` as

```
5
    3
        1
            <empty>
            <empty>
        4
            <empty>
            <empty>
    <empty>
```
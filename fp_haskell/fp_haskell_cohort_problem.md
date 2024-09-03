% introduction to Haskell

# Learning Outcomes
By the end of this class, you should be able to 

* Develop simple implementation in Haskell using List, Conditional, and Recursion
* Model problems and design solutions using Algebraic Datatype and Pattern Matching
* Compile and execute simple Haskell programs

# Exercise 1

Recall that the a fibonacci sequence can be defined as

```
1, 1, 2, 3, 5, ...
```

where the $n$-th number is equal to the sum of $(n-1)$-th and $(n-2)$-th numbers for $n>1$.

Mathematically, we define the $fib(n)$ function for natural number $n$ as follows

$$
fib(n) = \left [
    \begin{array}{ll}
    1 & {n \leq 1} \\
    fib(n-1) + fib(n-2) & {otherwise}
    \end{array}
    \right .
$$

Implement the `fib :: Int -> Int` function in Haskell based on the specification above.


# Exercise 2

Recall that in Haskell, $[1,2,3]$ denotes a list containing numbers 1, 2 and 3. $["hello", "world"]$ denotes a list of two strings.

Task 1. Using recursion in Haskell, write a function `len :: [a] -> Int` that takes a list  and return the number of elements in the list. 

Task 2. Can you rewrite a version using tailed-recursion?



# Exercise 3

Write a function `lift :: [a] -> [[a]]`, which takes in a list and returns a list whose elements are singleton lists containing elements from the input list. 
For instance, `lift [1,2,3]` should return `[[1],[2],[3]]`



# Exercise 4

Write a function `flatten :: [[a]] -> [a]`, which takes a list of lists and returns a flattened version of the input. For instance, `flatten [[1],[2],[3]]` should return `[1,2,3]`. 


# Exercise 5

Given the following specification of the merge sort algorithm, 

$$
mergesort(l) = \left [ 
    \begin{array}{ll}
        [] & {if\ l == []} \\
        [x] & {if\ l == [x]} \\
        merge(mergesort(l_1), mergesort(l_2)) & { split(l) == (l_1,l_2) } 
    \end{array}
    \right .
$$

where $split(l)$ splits a list into two lists, among which the size difference is less or equal to 1. 

$merge(l_1,l_2)$ merges two sorted lists

$$
merge(l_1,l_2) = \left [ 
    \begin{array}{ll}
     l_2 & {if\ l_1 == []} \\
     l_1 & {if\ l_2 == []} \\ 
     [hd(l_1)] \uplus merge(tl(l_1),l_2) & {if\ hd(l_1) <  hd(l_2)} \\
     [hd(l_2)] \uplus merge(l_1, tl(l_2)) & {if\ hd(l_1) \geq hd(l_2)}  
    \end{array}
    \right .
$$


Implement a function `mergesort` which sorts a list of integers in ascending order.

* For simplicity, we provide the definitions of `split` and `merge` functions in the project template. 


# Exercise 6

Implement a function `rotate :: [[a]] -> [[a]]` which rotates a 2D array  (actually it's a list of lists, whose elements having the same size) `l` 90-degree clock-wise.

For instance 
```hs
rotate [[1,2,3], [4,5,6], [7,8,9]]
```
should produce

```hs
[[7,4,1], [8,5,2], [9,6,3]] 
```

# Exercise 7 

Recall a binary search tree is a binary tree with the following property.

* For all non-leaf node $n$, $n$'s key (payload) is greater than all its left descendants', and less than than all its right descendants'.


1. Model a binary search tree whose keys are integers using algebraic data type, `BST`.
2. Implement an `insert` function which insert a key into a BST, it should have the following type.

```hs 
insert :: Int -- ^ key to be inserted
    -> BST    -- ^ input BST
    -> BST    -- ^ output BST
```

3. Implement a `find` function whch returns `True` when the key `k` is in the BST `t` and returns `False` otherwise.

```hs
find :: Int -- ^ key to be found
    -> BST  -- ^ input BST
    -> Bool
```

You should write your own test case based on your algebraic datatype.

% introduction to Scala

# Learning Outcomes
By the end of this class, you should be able to 

* Develop simple implementation in Scala using List, Conditional, and Recursion
* Model problems and design solutions using Algebraic Datatype and Pattern Matching
* Compile and execute simple Scala programs

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

Implement the above `fib(n)` function in Scala

Some test cases:

```scala
assert(fib(1) == 1)
assert(fib(5) == 8)
assert(fib(10) == 89)
```

Answer:
```scala
def fib(n:Int):Int = n match {
    case 0 => 1
    case 1 => 1
    case _ => fib(n-1) + fib(n-2)
}
```


# Exercise 2

Recall that in Scala, $List(1,2,3)$ denotes a list containing numbers 1, 2 and 3. $List("hello", "world")$ denotes a list of two strings.

Using recursion in Scala, write a function `length(l)` that takes a list `l` and return the number of elements in `l`. 

Some test cases:
```
assert(length(Nil) == 0)
assert(length(List(1,2,3)) == 3)
assert(length(List('a','b','c','d')) == 4)
```

Answer:
```scala
def length[A](l:List[A]):Int = l match {
    case Nil => 0
    case (x::xs) => 1 + length(xs)
}
```

Can you rewrite a version using tailed-recursion?

Answer:

```scala
def length_tail_rec[A](l:List[A]):Int = {
    def go(il:List[A], count:Int):Int = il match {
        case Nil => count
        case (x::xs) => go(xs, 1+count)
    }
    go(l, 0)
}
```


# Exercise 3

Write a function `lift(l)`, which takes in a list `l` and returns a list whose elements are singleton lists containing elements from `l`. For instance, `lift(List(1,2,3))` should return `List(List(1),List(2),List(3))`


Some test cases:

```scala
assert(lift(List(1,2,3)) == List(List(1), List(2), List(3)))
assert(lift(List('a','b','c','d')) == List(List('a'), List('b'), List('c'), List('d')))
```

Answer:

```scala
def lift[A](l:List[A]):List[List[A]] = l.map( x => List(x))
```


# Exercise 4

Write a function `flatten(l)`, which takes a list of lists `l` and returns a flattened version of `l`. For instance, `flatten(List(List(1),List(2),List(3)))` shoudl return `List(1,2,3)`. 

Some test cases:

```scala
val l = List(1,2,3) 
assert(flatten(lift(l)) == l)
```

Answer:

```scala
def flatten[A](l:List[List[A]]):List[A] = l.foldLeft(Nil:List[A])( (flt, il) => flt ++ il)
```


Question: would it be possible for `l` to contain lists of different nested level?


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

$merge(l1,l2)$ merges two sorted lists

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


Implement a function `mergesort(l)` which sorts a list of integers `l` in ascending order.

Some test cases:

```scala
val l = List(3,17,8,9,11,200,0,5)
assert(merge_sort(l) == List(0, 3, 5, 8, 9, 11, 17, 200))
```


Answer:

```scala
def merge(l1:List[Int], l2:List[Int]):List[Int] = (l1,l2) match {
    case (Nil, _) => l2
    case (_, Nil) => l1
    case (x::xs, y::ys) if (x < y) => x::merge(xs,y::ys) 
    case (x::xs, y::ys)            => y::merge(x::xs,ys)
}

def split(l:List[Int]):(List[Int],List[Int]) = {
    def go(il:List[Int], ols:(List[Int], List[Int])):(List[Int], List[Int]) = il match {
        case Nil => ols
        case (x::xs) => ols match {
            case (ol1, ol2) => go(xs, (x::ol2, ol1))
        }
    }
    go(l, (Nil,Nil))
}

def merge_sort(l:List[Int]):List[Int] = l match {
    case Nil => Nil
    case List(x) => List(x)
    case _ => split(l) match {
        case (l1, l2) => {
            val l1_sorted = merge_sort(l1)
            val l2_sorted = merge_sort(l2)
            merge(l1_sorted, l2_sorted)
        }
    }
}
```


# Exercise 6

Implement a function `rotate(l)` which rotates a 2D array  (actually it's a list of lists, whose elements having the same size) `l` 90-degree clock-wise.

For instance 

```scala
val l = List(
    List(1,2,3),
    List(4,5,6),
    List(7,8,9)
)

val l_rotated = List(
    List(7,4,1),
    List(8,5,2),
    List(9,6,3)
)

assert(l_rotated == rotate(l))
```


```scala
def rotate[A](l:List[List[A]]):List[List[A]] = l match {
    case Nil => Nil
    case (Nil::_) => Nil
    case (_::_) => {
        val hds = l.map( x => x.head )
        val tls = l.map( x => x.tail )
        hds.reverse::rotate(tls)
    }
}

```


# Exercise 7 


Recall a binary search tree is a binary tree with the following property.

* For all non-leaf node $n$, $n$'s key (payload) is greater than all its left descendants', and less than than all its right descendants'.


1. Model a binary search tree whose keys are integers using algebraic data type, `BST`.
2. Implement an `insert(k:Int, t:BST):BST` function which insert a key `k` into a BST `t`.
3. Implement a `find(k:Int, t:BST):Boolean` function whch returns `true` when the key `k` is in the BST `t` and returns `false` otherwise.

You should write your own test case based on your algebraic datatype.


Answer:
```scala
enum BST {
    case Empty
    case Node(key:Int, left:BST, right:BST)
}

import BST.*
def insert(k:Int, t:BST):BST = t match {
    case Empty => Node(k, Empty, Empty)
    case Node(k1, left, right) if k == k1 => t
    case Node(k1, left, right) if k < k1  => {
        val left_u = insert(k, left)
        Node(k1, left_u, right)
    }
    case Node(k1, left, right) => {
        val right_u = insert(k, right)
        Node(k1, left, right_u)
    }
}

def find(k:Int, t:BST):Boolean = t match {
    case Empty => false
    case Node(k1, left, right) if k == k1 => true
    case Node(k1, left, right) if k < k1  => find(k, left)
    case Node(k1, left, right)            => find(k, right)
}
```


```scala
val t0 = Empty
val t1 = List(3,17,8,9,11,200,0,5).foldLeft(t0)((t,k) => insert(k,t))
assert(t1 == Node(3,Node(0,Empty,Empty),Node(17,Node(8,Node(5,Empty,Empty),Node(9,Empty,Node(11,Empty,Empty))),Node(200,Empty,Empty))))
assert(find(0,t1))
assert(find(11,t1))
```

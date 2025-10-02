% Parametric Polymorphism and Adhoc Polymorphism 


# Learning Outcome

By this end of this lesson, you should be able to 

* develop parametrically polymorphic Scala code using Generic, Algebraic Datatype
* safely mix parametric polymoprhism with adhoc polymoprhism (overloading) using type classes 
* develop generic programming style code using `Functor` type class.
* make use of `Option` and `Either` to handle and manipulate errors and exceptions. 

# Exercise 1

Consider the following built-in type class 

```scala
trait Ordering[A] { 
    def compare(x:A,y:A):Int // -1 : less than, 0 : equal, 1 greater than
}
```

We can define a type class instance for ordering among interger values.

```scala
given intOrd:Ordering[Int] = new Ordering[Int] {
    def compare(x:Int, y:Int):Int = if (x < y) {
        -1
    } else {
        if (x > y) {
            1
        } else { 0 }
    }
}
```

Define a type class instance `Ordering[Boolean]`, such that `true < false`

Test cases:
```scala
assert(booleanOrd.compare(true,false) == -1)
assert(booleanOrd.compare(true,true) == 0)
assert(booleanOrd.compare(false,true) == 1)
assert(booleanOrd.compare(false,false) == 0)
```



# Exercise 2

Define a polymorphic quicksort function which makes use of `Ordering` type class context.


Test cases:

```scala
assert(qsort(List(3,5,2,1)) == List(1,2,3,5))
assert(qsort(List(false, false, true)) == List(true, false, false))
```


# Exercise 3

Given the following algebraic data type of a polymorphic binary search tree, and a type class `Ordering`. 

```scala
enum BST[+A] {
    case Empty
    case Node(v:A, lft:BST[A], rgt:BST[A])
}
```

Complete the following `insert` for `BST` which makes use of the `Ordering[A]` type class context.

```scala
def insert[A](k:A, t:BST[A])(using orda:Ordering[A]):BST[A] = t // fixme
```

Test cases:

```scala
val t0 = insert(3, Empty)
assert(t0 == Node(3,Empty,Empty))
val t1 = insert(5, t0)
assert(t1 == Node(3,Empty, Node(5, Empty, Empty)))

val t2 = insert(true, Empty)
val t3 = insert(false, insert(true, t2))
assert(t3 == Node(true, Empty, Node(false, Empty, Empty)))
```


# Exercise 4

Recall in the lecture, we defined the following `Foldable` type class

```scala
trait Foldable[T[_]]{
    def foldLeft[A,B](t:T[B])(acc:A)(f:(A,B)=>A):A
}

given listFoldable:Foldable[List] = new Foldable[List] {
    def foldLeft[A,B](t:List[B])(acc:A)(f:(A,B)=>A):A = t.foldLeft(acc)(f)
}
```

Modify the definition of the `Foldable` type class to include the function `foldRight`, provide the type class instances for `List` and `BST`.


Test cases:

```scala
val l = List(1,2,3,4)
assert(listFoldable.foldLeft(l)(0)(x:Int,y:Int) => x - y) == -10)
// (((0-1)-2)-3)-4
assert(listFoldable.foldRight(l)(0)(x:Int,y:Int) => x - y) == -2)
// 1-(2-(3-(4-0)))

import BST.*
val b = Node(5, Node(3, Node(1, Empty, Empty), Node(4,Empty, Empty)), Node(7, Empty, Empty))

assert(bstFoldable.foldLeft(b)("")(x:String, y:Int) => x + " " + y.toString)  == " 5 3 1 4 7")
// C-L-R pre order
assert(bstFoldable.foldRight(b)("")(x:Int, y:String) => x.toString + " " + y)  == "5 7 3 4 1 ")

// C-R-L mirrored pre-order
```



# Exercise 5

Define a `subtree[A](k:A,bst:BST[A]):Option[BST[A]]` function which search for the sub-tree rooted at the given key `k` in a BST `bst`.

```scala
def subtree[A](k:A, bst:BST[A])(using orda:Ordering[A]):Option[BST[A]] = None // TODO: fixme
```
Test cases:

```scala
val t0 = insert(3, Empty)
val t1 = insert(5, t0)
val t2 = insert(4, t1)
assert(subtree(5,t2) == Some(Node(5,Node(4,Empty,Empty),Empty)))
```

# 50.054 - Applicative and Monad


## Learning Outcomes



1. Describe and define derived type class
2. Describe and define Applicative Functors
3. Describe and define Monads
4. Apply Monad to in design and develop highly modular and resusable software.


## Exercise 1 


Define the type class instance `Functor[Option]`. Then verify that it satisfies the Functor Laws.




## Exercise 2

Given the following type lambda of a pair type.
```scala
type PP = [B] =>> [A] =>> (A,B)
```

Define the functor of instance of `Functor[PP[C]]` called `pairFunctor`.


### Test Cases

```scala
val pair = (1, "A")
val expected = (2, "A")
val result = pairFunctor.map(pair)(x => x + 1)
assert(expected == result)
```


## Exercise 3

Show that for all Applicative instance $a$ satisfying the Applicative Laws implies that $a$ satisfies the Functor Laws.


## Exercise 4

Consider the following data type, complete the implementation of `map` and `flatMap`.

```scala
case class Mk[S,A]( f : (S =>(S, Option[A]) )) {
    def map[B](f:A=>B):Mk[S,B] = // TODO
    def flatMap[B](f:A=>Mk[S,B]):Mk[S,B] = // TODO
}
```

### Test Cases

```scala
val m = Mk( s => (s, Some(1)) )
val result:Option[Int] = runMk(m.map( x => x + 1))(()) 
val expected:Option[Int] = Some(2)
assert(expected == result)
```

```scala
val m = Mk( s => (s, Some(1)) )
val result:Option[Int] = runMk(m.flatMap( x => Mk(s => (s,Some(x + 1)))))(())
val expected:Option[Int] = Some(2)
assert(expected == result)
```

For more test cases, refer to the project test folder

## Exercise 5

Continue from the previouse quesiton, 

```scala
type MkM = [S] =>> [A] =>> Mk[S,A]
```

Define a derived type class `MkMonad[S]` that extends `Monad[MkM[S]]`

```scala
trait MkMonad[S] extends Monad[MkM[S]] {
    // TODO
}
```

### Test Cases

Let 
```scala
def runMk[S,A](mk:Mk[S,A])(s:S):Option[A] = mk match {
    case Mk(f) => f(s) match {
        case (_,o) => o
    }
}
```

```scala
given mkMonadInt:MkMonad[Int] = new MkMonad[Int]{} 
def incrState(using i:MkMonad[Int]):Mk[Int,Int] = for {
    x <- i.get
    _ <- i.set(x+1)
    y <- i.get
} yield y
val result:Option[Int] = runMk(incrState)(1)
val expected:Option[Int] = Some(2)
assert(expected == result)
```

For more test cases, refer to the project test folder

## Exercise 6



Consider the following code

```scala
enum BTree[+A] {
    case Empty
    case Node(v:A, lft:BTree[A], rght:BTree[A])
}

import BTree.*;

val tree = Node(5, Node(3, Node(1, Empty, Empty), Node(4, Empty, Empty)), Empty)

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

### Test Cases


```scala
val tree = Node(5, Node(3, Node(1, Empty, Empty), Node(4, Empty, Empty)), Empty)
val expected = """5
3
    1
        <empty>
        <empty>
    4
        <empty>
        <empty>
<empty>"""
rmPrint(tree) match {
    case Reader(run) => { 
        val result = run(Env(0))
        assert(expected == result)
    }
}   
```
# 50.054 - Applicative and Monad


## Learning Outcomes


1. Describe and define derived type class
2. Describe and define Applicative Functors
3. Describe and define Monads
4. Apply Monad to in design and develop highly modular and resusable software.


## Exercise 1 


Define the type class instance `Functor[Option]`. Then verify that it satisfies the Functor Laws.


### Answer
```scala
given optFunctor:[Functor[Option]] = new Functor[Option] {
    def map[A,B](fa: Option[A])(f:A=>B):Option[B] = fa match {
        case None => None
        case Some(a) => Some(f(a))
    }
}
```

First to prove that it satisfies the Identity Law.

```scala
i=>map(i)(x=>x) ---> 
i match {
    case None => None
    case Some(a) => Some(x=>x(a))
}
```

1) Subcase `i` is `None`. We are done, since the above yields `None` and `(x=>x)(None) ---> None`. 
2) Subcase `i` is `Some(a)`. The above is evaluated to `Some(a)` and `(x=>x)(Some(a)) --> Some(a)`.

Secondly, we prove that it satisfies the Composition Law.

The LHS

```scala
i=>map(i)(f.compose(g)) --->
k=>map(k)(f.compose(g)) --->
k match {
    case None => None
    case Some(a) => Some((f.compose(g))(a))
}
```

The RHS
```scala
(i => map(i)(f)).compose((j=>map(j)(g))) --->

k => ((i => map(i)(f)))((j=>map(j)(g))(k)) ---> 

k => map(map(k)(g))(f)
```

1) Subcase `k` is `None`. Trivial. 
2) Subcase `k` is `Some(a)`. LHS is `Some((f(g(a)))`. 

RHS is 

```scala
map(map(Some(a))(g)(f)) -->
map(Some(g(a)))(f) -->
Some(f(g(a)))
```

We have verified the optional functor instance satisfies the composition law.



## Exercise 2

Given the following type lambda of a pair type.
```scala
type PP = [B] =>> [A] =>> (A,B)
```

Define the functor of instance of `Functor[PP[C]]`.


### Test Cases

```scala
val pair = (1, "A")
val expected = (2, "A")
val result = pairFunctor.map(pair)(x => x + 1)
assert(expected == result)
```

### Answer

```scala

type PP = [B] =>> [A] =>> (A,B)

given pairFunctor[C]:Functor[PP[C]] = new Functor[PP[C]]  {
    def map[A,B](fa:(A,C))(f:A=>B):(B,C) = fa match {
        case (a,c) => (f(a),c)
    }
}

```


## Exercise 3

Show that for all Applicative instance $a$ satisfying the Applicative Laws implies that $a$ satisfies the Functor Laws.


### Answer

First we show that $a$ satisfies the Functor Identity Law.

LHS of the Functor Identity Law
```scala
i=>map(i)(x => x)  \equiv // by def of map
i=>ap(pure(x=>x))(i) \equiv // eta reduction
ap(pure(x=>x))    \equiv  // by AP identity law
x => x   // RHS of Functor ID Law
```

Second we show that $a$ staisfies the Functor Composition LAw.

LHS of the Functor Composition Law
```scala
i=>map(i)(f.compose(g))  \equiv // by def of map
i=>ap(pure(f.compose(g)))(i)
```

RHS of the Functor Composition Law
```scala
(i=>map(i)(f)).compose(j=>map(j)(g))  \equiv // by def of map
(i=>ap(pure(f))(i)).compose(j=>ap(pure(g))(j))  \equiv  // by def of compose
x=>( (i=>ap(pure(f))(i))( 
        (j=>ap(pure(g))(j))(x)
        ) 
    )             \equiv // beta reduction
x=>( (i=>ap(pure(f))(i))(
        ap(pure(g))(x)
       )
    )             \equiv // beta reduction
x=>( ap(pure(f))(ap(pure(g))(x)))  \equiv // by AP composition Law

x => ( ap(ap(pure(h=> h.compose))(pure(f))(pure(g)))(x)) \equiv // by AP Homomorphism Law (the inner most ap(...)())

x => (ap(ap(pure(f.compose))(pure(g)))(x)) \equiv// by AP Homomorphism Law

x=> (ap(pure(f.compse(g)))(x))
```

We have shown that LHS $\equiv$ RHS.

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

### Answer

```scala
case class Mk[S,A]( f : (S =>(S, Option[A]) )) {
    def map[B](f:A=>B):Mk[S,B] = this match {
        case Mk(ssoa) => Mk( s=> ssoa(s) match {
            case (s1,None) => (s1, None)
            case (s1,Some(a)) => (s1, Some(f(a)))
        }) 
    }
    def flatMap[B](f:A=>Mk[S,B]):Mk[S,B] = this match {
        case Mk(ssoa) => Mk( s => ssoa(s) match {
            case (s1, None) => (s1, None)
            case (s1, Some(a)) => f(a) match {
                case Mk(ssob) => ssob(s1) 
            }
        })
    }
}
```


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

### Answer

```scala
type MkM = [S] =>> [A] =>> Mk[S,A]

trait MkMonad[S] extends Monad[MkM[S]] {
    override def pure[A](v:A):Mk[S,A] = Mk( s=> (s,Some(v)))
    override def bind[A,B](
        fa:Mk[S,A]
        )(
            ff:A => Mk[S,B]
        ):Mk[S,B] = fa.flatMap(ff)
    def get:Mk[S, S] = Mk(s => (s,Some(s)))
    def set(v:S):Mk[S,Unit] = Mk(s => (v,Some(())))
}

```


# Exercise 6


Consider the following code

```scala
enum BTree[+A] {
    case Empty
    case Node(v:A, lft:BTree[A], rght:BTree[A])
}

import BTree.*;

val tree = Node(5, Node(3, Node(1, Empty, Empty), Node(4, Empty, Empty)), Empty)

```
Use reader monad to print the tree `tree` as

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

### Answer

```scala
case class Env(indent:Int)

given EnvReader:ReaderMonad[Env] = new ReaderMonad[Env] {}


def indentEnv(env:Env):Env = env match {
    case Env(x) => Env(x + 4);
}


def rmPrint(t:BTree[Int])(using pr:ReaderMonad[Env]):Reader[Env, String] = t match {
    case Empty => for {
        env <- pr.ask
        indent <- env match {
            case Env(i) => pr.pure(" " * i)
        }
    } yield (indent ++ "<empty>")
    case Node(v, lt, rt) => for {
        env <- pr.ask
        indent <- env match {
            case Env(i) => pr.pure(" " * i)
        }
        ls <- pr.local(indentEnv)(rmPrint(lt))
        rs <- pr.local(indentEnv)(rmPrint(rt))
    } yield (indent ++ v.toString() ++ "\n" ++ ls ++ "\n" ++ rs)
}
def main(args:Array[String]):Unit = {
    rmPrint(tree) match {
        case Reader(run) => { 
            val s = run(Env(0))
            println(s)
        }
    }   
}
```
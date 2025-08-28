# 50.054 - syntax analysis


## Learning Outcome 

By the end of this class, you should be able to 

* Implement a top-down recursive parser with backtracking
* Implement a top-down recursive parser with on-demand backtracking 


## Lexing 

Let's consider the math expression grammar

```
<<grammar 4>> 
E::= T + E
E::= T
T::= T * F 
T::= F
F::= i    
```

The terminal symbols are `+`, `*` and `i`. 

We define the following algebraic datatype to model the lexical tokens.

```scala
enum LToken { // lexical Tokens
    case IntTok(v: Int)
    case PlusTok
    case AsterixTok
}
```


Following the same idea mentioned in the lecture note, we make use of the Scala regex library to implement a lexer.

```scala
def lex(src:String):Either[List[LToken], Error] = {
    def go(src:String, acc:List[LToken]):Either[List[LToken], Error] = {
        if (src.length == 0)  {
            Left(acc)
        } else {
            lex_one(src) match {
                case Right(error) => Right(error)
                case Left((ltoken, rest)) => go(rest, acc++List(ltoken))
            }
        }
    }
    go(src, List())
}

val integer = raw"(\d+)(.*)".r
val plus = raw"(\+)(.*)".r
val asterix = raw"(\*)(.*)".r

import LToken.*
def lex_one(src:String):Either[(LToken, String), Error] = src match {
    case integer(s, rest) => Left((IntTok(s.toInt), rest))
    case plus(_, rest) => Left((PlusTok, rest))
    case asterix(_, rest) => Left((AsterixTok, rest))
    case _ => Right(s"lexer error: unexpected token at ${src}")
}
```

A simple test case to check that our implementation is working.

```scala
test("test_lex") {
    val s = "1+2*3"
    val result = lex(s)
    assert(
        result == Left(
        List(IntTok(1), PlusTok, IntTok(2), AsterixTok, IntTok(3))
        )
    )
}
```


## Top-down parsing

In this cohort problem we are going to focus on Top-down parsing.

### Abstract Syntax Ttree 
To implement top-down parsing, we first consider how to represent a parse tree in Scala. 
It's natural to implement the parse trees in terms of some algebraic datatype. 
The Grammar 4 can be encoded with the following Scala enum type.

```scala
enum Exp {
    case TermExp(t:Term)
    case PlusExp(t:Term, e:Exp)
}

enum Term {
    case FactorTerm(f:Factor)
    case MultTerm(t:Term, f:Factor)
}

case class Factor(v:Int)
```

### Left Recursion Elimination

Recall Grammar 4 defined above contains some left recursion. 

To eliminate the left recursion, we apply the same trick by rewriting left recursive grammar rules
$$
\begin{array}{rcl}
N & ::= & N\alpha_1 \\
& ... & \\
N & ::= & N\alpha_n \\
N & ::= & \beta_1 \\ 
& ... & \\
N & ::= & \beta_m 
\end{array}
$$

into

$$
\begin{array}{rcl}
N & ::= & \beta_1 N' \\
& ... & \\
N & ::= & \beta_m N' \\
N' & ::= & \alpha_1 N' \\ 
& ... & \\
N' & ::= & \alpha_n N' \\
N' & ::= & \epsilon
\end{array}
$$

Grammar 4 can be rewritten into

```
E  ::= T + E
E  ::= T
T  ::= FT'
T' ::= *FT'
T' ::= epsilon
F  ::=i
```

None of the production rules above contains common leading terminal symbols, hence there is no need to apply left-factorization.

Note that in the above Grammar 4 with left recursion eliminated, the only rules affected are thos with non-terminal `T`,

Hence we only need to added the following enum type

### Additional Abstract Syntax Tree


We could model it using Algebraic data type.
```scala
case class TermLE(f:Factor, tp:TermLEP)

enum TermLEP {
    case MultTermLEP(f:Factor, tp:TermLEP)
    case Eps
}
```

The main idea is when parsing a `Term`, instead of parsing directly, we parse a `TermLE` then convert it back to `Term`.


```scala
List(IntTok(1), PlusTok, IntTok(2), AsterixTok, IntTok(3))
```
A parser method `parse` should generate

```scala
PlusExp(FactorTerm(Factor(1)),TermExp(MultTerm(FactorTerm(Factor(2)),Factor(3))))
```
where 

* sub term `IntTok(1)` was first parsed as `TermLE(Factor(1), Eps)` then converted to `FactorTerm(Factor(1))`, and 
* sub term `IntTok(2), AsterixTok, IntTok(3)` was first parsed as `TermLE(Factor(2), MultTerm(Factor(3), Eps))` and converted to `MultTerm((Factor(2), Factor(3)))`.



### Parser Combinator with Backtracking

We consider implementing the naive top-down recursive parser in Scala. 
Let's start with the simplest cases. Let's say we would like to write a parsing function that takes a list of lexical tokens and "consumes" a token, then return the rest.

```scala
enum Result[A] {
  case Failed(msg:String)
  case Ok(v:A)
}

def item(toks:List[LToken]):Result[(LToken, List[Token])] = toks match {
  case Nil => Failed("item() is called with an empty input")
  case (t::ts) => Ok((t, ts))
}
```
We define a variant of the `Option` datatype, `Result` which is either a failure with an error message, or an "Ok" result. The `item` function is what we would like to implement. It returns the extracted leading token with the rest of input if the input is non-empty, and signals a failure otherwise. 

Apply the same idea we could define a conditional parsing function.

```scala
def sat(toks:List[LToken])(p:LToken => Boolean):Result[(LToken, List[Token])] = toks match {
  case Nil => Failed("sat() is called with an empty input")
  case (t::ts) if p(t) => Ok((t, ts))
  case (t::ts)         => Failed("sat() is called with an input that does not satisfy the input predicate.")
}
```

We may want to combine these basic parsing functions to form a larger parsing task, e.g.

```scala
def aBitMoreComplexParser(toks:List[LToken]):Result[(LToken,List[LToken])] = 
  item(toks) match {
    case Failed(msg) => Failed(msg)
    case Ok((_, toks2)) => sat(toks2)(t => t match {
      case AsterixTok => true
      case _          => false
    }, "Expecting an asterix." )
  }
```


In the above, we define a parsing task which skips the first token and searches for the following asterix. We could imagine that to build a practical parser, we would need many of the basic parsing functions like `sat` and `item`, and combine them.

What we can observe from the above is that there are some similarity between `sat` and `item`, i.e. they both take in a list of tokens and returns the remaining tokens. If we view the lists of tokens as states, we could think of using the State Monad. We would also need this top-down parser to be able to backtrack in case of parsing failures. Recall the `MonadError` type class

```scala
trait Monad[F[_]] extends Applicative[F] {
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
        bind(ff)((f: A => B) => bind(fa)((a: A) => pure(f(a))))
}
trait MonadError[F[_], E] extends Monad[F] with ApplicativeError[F, E] {
    override def pure[A](v: A): F[A]
    override def raiseError[A](e: E): F[A]
    override def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
}
```
We include one extra method `handleErrorWith` which takes a functor `fa`
and executes it, in case of error being raised in `fa`, it applies `f` to the error to recover from the error.

We define the `Parser` case class as follows, similar to the `State` case class in the State Monad, except that we fix the state to be `List[T]`, where `T` is a parametric type for tokens, for instance `LToken`.

```scala
case class Parser[T, A](p: List[T] => Result[(A, List[T])]) {
    def map[B](f: A => B): Parser[T, B] = this match {
        case Parser(ea) =>
            Parser(env =>
                ea(env) match {
                    case Failed(err)   => Failed(err)
                    case Ok((a, env1)) => Ok((f(a), env1))
                }
            )
    }
    def flatMap[B](f: A => Parser[T, B]): Parser[T, B] = this match {
        case Parser(ea) =>
            Parser(env =>
                ea(env) match {
                    case Failed(err) => Failed(err)
                    case Ok((a, env1)) =>
                        f(a) match {
                            case Parser(eb) => eb(env1)
                        }
                }
            )
    }
}

def run[T, A](
    parser: Parser[T, A]
)(env: List[T]): Result[(A, List[T])] = parser match {
    case Parser(p) => p(env)
}

type ParserM = [T] =>> [A] =>> Parser[T, A]
```

Next we define an instance of `MonadError[ParserM[T], Error]`.

```scala
given parsecMonadError[T]: MonadError[ParserM[T], Error] =
    new MonadError[ParserM[T], Error] {
        override def pure[A](a: A): Parser[T, A] =
            Parser(cs => Ok((a, cs)))
        override def bind[A, B](
            fa: Parser[T, A]
        )(f: A => Parser[T, B]): Parser[T, B] = fa.flatMap(f)
        override def raiseError[A](e: Error): Parser[T, A] =
            Parser(env => Failed(e))
        override def handleErrorWith[A](
            fa: Parser[T, A]
        )(f: Error => Parser[T, A]): Parser[T, A] = fa match {
            case Parser(ea) =>
                Parser(env =>
                    ea(env) match {
                        case Failed(err) => run(f(err))(env)
                        case Ok(v)       => Ok(v)
                    }
                )
        }
    }
```

With the Monad instance in-place, we can re-define the `item()` and `sat()` methods in monadic style.

```scala
def item[T]: Parser[T, T] =
    Parser(env => {
        val toks = env
        toks match {
            case Nil =>
                Failed(s"item() is called with an empty token stream")
            case (c :: cs) => Ok((c, cs))
        }
    })

def sat[T](p: T => Boolean, err:String=""): Parser[T, T] = Parser(toks =>
    toks match {
        case Nil => Failed(s"sat() is called with an empty token stream. ${err}")
        case (c :: cs) if p(c) => Ok((c, cs))
        case (c :: cs) =>
            Failed(s"sat() is called with a unsatisfied predicate with ${c}. ${err}")
    }
)
```
In the `sat` combinator, we pass in two arguments, the predicate `p` and an optional `err` error parameter, which is default to empty string.


More importantly, we could define more generic and useful combinators

```scala
def choice[T, A](p: Parser[T, A])(q: Parser[T, A])(using
    m: MonadError[ParserM[T], Error]
): Parser[T, A] = m.handleErrorWith(p)(e => q)
```

The `choice` combinator takes two parsers `p` and `q`. It tries to run `p`. In case `p` fails, it backtracks (by restoring the original state) and runs `q`. 


Now we can make use of `choice` to define an `optional` combinator
```scala
def optional[T, A](pa: Parser[T, A]): Parser[T, Either[Unit, A]] = {
    val p1: Parser[T, Either[Unit, A]] = for (a <- pa) yield (Right(a))
    val p2: Parser[T, Either[Unit, A]] = Parser(toks => Ok((Left(()), toks)))
    choice(p1)(p2)
}
```

`optional` takes a parser `pa` and tries to execute it with the current input. If it fails, it restores the original state and returns `Unit`.


Let's try to write and a parser for the simple arithmetic expression 

Recall the nice property of a top-down parser is that the parser is correspondent to the top-down traversal of the production rules.

Recall the grammar 4 of Math expression with left recursion.
```
E::= T + E
E::= T
T::= T * F 
T::= F
F::= i    
```


```scala
def parseExp:Parser[LToken, Exp] = 
    choice(parsePlusExp)(parseTermExp)

def parsePlusExp:Parser[LToken, Exp] = for {
    t <- parseTerm
    plus <- parsePlusTok
    e <- parseExp
} yield PlusExp(t, e)

def parseTermExp:Parser[LToken, Exp] = for {
    t <- parseTerm
} yield TermExp(t)
```

Up to this point we are ok as production rules with `E` on the LHS are not left recursive.
It gets tricky when paarsing `T` which contains left recursion. Recall the modified grammar of 
`T` having left-recursion eliminated.

```
T  ::= FT'  
T' ::= *FT'
T' ::= epsilon
```

In terms of Scala enum type, we refer to them as `TermLE` and `TermLEP`.
Hence the parser `parserTerm` has to be defined in terms of `parseTermLE`, then 
convert the result of `TermLE` back to `Term`

```scala
def parseTerm:Parser[LToken, Term] = for {
    tle <- parseTermLE
} yield fromTermLE(tle)
```

Where `parseTermLE` can be implemented using parsec, 

```scala
def parseTermLE:Parser[LToken, TermLE] = for {
    f <- parseFactor
    tp <- parseTermP 
} yield TermLE(f, tp)

def parseTermP:Parser[LToken, TermLEP] = for {
    omt <- optional(parseMultTermP)
} yield { omt match {
    case Left(_) => Eps
    case Right(t) => t
}}
    
def parseMultTermP:Parser[LToken, TermLEP] = for {
    asterix <- parseAsterixTok
    f <- parseFactor
    tp <- parseTermP
} yield MultTermLEP(f, tp)

def parseFactor:Parser[LToken, Factor] = for {
    i <- parseIntTok
    f <- someOrFail(i)( itok => itok match {
        case IntTok(v) => Some(Factor(v))
        case _         => None
    })("parseFactor() fail: expect to parse an integer token but it is not an integer.")
} yield f

def parsePlusTok:Parser[LToken, LToken] = sat ((x:LToken) => x match {
    case PlusTok => true
    case _       => false
}, "Expecting a + symbol")


def parseAsterixTok:Parser[LToken, LToken] = sat ((x:LToken) => x match {
    case AsterixTok => true
    case _          => false
}, "Expecting a * symbol")

def parseIntTok:Parser[LToken, LToken] = sat ((x:LToken) => x match {
    case IntTok(v) => true
    case _         => false
}, "Expecting an int token")
```

Finally the `TermLE` to `Term` conversion is an inversed in order traversal, as the parse tree of `TermLE`

```
    T
   / \
  f   T'
     /|\
    * f T'
       /|\
      * f T'
          |
         eps
```

and the parse tree of `Term` is

```
        T
       /|\
      T * f
     /| \
    T * f 
    |
    f
```

The implementation can be found as follows,

```scala
def fromTermLE(t:TermLE):Term = t match {
    case TermLE(f, tep) => fromTermLEP(FactorTerm(f))(tep)
} 
def fromTermLEP(t1:Term)(tp1:TermLEP):Term = tp1 match {
    case Eps => t1 
    case MultTermLEP(f2, tp2) => {
        val t2 = MultTerm(t1, f2)
        fromTermLEP(t2)(tp2)
    }
}
```

And here is some test cases

```scala
test("test_parse") {
    // val s = "1+2*3"
    val toks = List(IntTok(1), PlusTok, IntTok(2), AsterixTok, IntTok(3))
    val result = BacktrackParsec.run(parseExp)(toks)
    val expected = PlusExp(FactorTerm(Factor(1)),TermExp(MultTerm(FactorTerm(Factor(2)),Factor(3))))
    result match {
        case Ok((t, Nil)) =>  assert(t == expected)
        case _ => assert(false)
    }
}
```


## Exercise 1


Recall the JSON syntax grammar

```
J  ::= i | 's' | [] | [IS] | {NS}
IS ::= J,IS | J
NS ::= N,NS | N
N ::= 's':J
```

Implement a lexer of JSON using Scala's regex library. 
There is some skeleton code given in the project stub. Your task is to complete the missing parts and make sure it passes the test cases.

## Exercise 2

Implement a parser of JSON. Is the grammar suitable for top-down parsing? 
There is some skeleton code given in the project stub. Your task is to 
complete the missing parts and make sure it passes the test cases.

```scala
test("test_json_empty_list_parse") {
    val input = List(LBracket, RBracket)
    val result = BacktrackParsec.run(parseJList)(input)
    val expected = JsonList(Nil) 
    result match {
        case Ok((t, Nil)) =>  assert(t == expected)
        case rest => {
            println(rest)
            assert(false)
        }
    }
}

test("test_json_list_parse") {
    val input = List(LBracket, IntTok(1), Comma, IntTok(2), RBracket)
    val result = BacktrackParsec.run(parseJList)(input)
    val expected = JsonList(List(IntLit(1),IntLit(2))) 
    result match {
        case Ok((t, Nil)) =>  assert(t == expected)
        case rest => {
            println(rest)
            assert(false)
        }
    }
}
    
test("test_json_parse") {
    // {'k1':1,'k2':[]}
    val input = List(LBrace,SQuote,StrTok("k1"),SQuote,Colon,IntTok(1),Comma,SQuote,StrTok("k2"),SQuote,Colon,LBracket, RBracket,RBrace)
    val result = BacktrackParsec.run(parseJSON)(input)
    val expected = JsonObject(
        Map(
            "k1" -> IntLit(1),
            "k2" -> JsonList(Nil)
    ))
    result match {
        case Ok((t, Nil))  =>  assert(t == expected)
        case rest => {
            println(rest)
            assert(false)
        }
    }
}

```


## Exercise 3

Can you define a Parser combinator that computes all parse trees?


## Exercise 4

Eliminate the left-recursion of the following grammar.

```
X ::= XXa | Y
Y ::= Yb | c
```



## LL(1) Parsing

Recall that back-tracking based top-down parser could be inefficient.
One possible solution is to use `LL(1)`. A grammar is said to be in `LL(1)` if its predictive parsing table does not contain conflicts.

A top-down predictive parsing table could be constructed by computing the $null$, $first$ and $follow$ sets.

$null(\overline{\sigma},G)$ checks whether the language denoted by $\overline{\sigma}$ contains the empty sequence.
$$
\begin{array}{rcl}
null(t,G) & = & false \\ 
null(\epsilon,G) & = & true \\ 
null(N,G) & = & \bigvee_{N::=\overline{\sigma} \in G} null(\overline{\sigma},G) \\ 
null(\sigma_1...\sigma_n,G) & = & null(\sigma_1,G) \wedge ... \wedge null(\sigma_n,G)
\end{array}
$$

$first(\overline{\sigma},G)$ computes the set of leading terminals from the language denotes by $\overline{\sigma}$.

$$
\begin{array}{rcl}
first(t,G) & = & \{t\} \\ 
first(N,G) & = & \bigcup_{N::=\overline{\sigma} \in G} first(\overline{\sigma},G) \\ 
first(\sigma\overline{\sigma},G) & = & 
  \left [ 
    \begin{array}{ll} 
      first(\sigma,G) \cup first(\overline{\sigma},G) & {\tt if}\ null(\sigma,G) \\ 
      first(\sigma,G) & {\tt otherwise} 
      \end{array} 
  \right . 
\end{array}
$$

$follow(\sigma,G)$ finds the set of terminals that immediately follows symbol $\sigma$ in any derivation derivable from $G$.

$$
\begin{array}{rcl}
follow(\sigma,G) & = & \bigcup_{N::=\overline{\sigma}\sigma{\overline{\gamma}} \in G} 
  \left [ 
    \begin{array}{ll}
      first(\overline{\gamma}, G) \cup follow(N,G) & {\tt if} null(\overline{\gamma}, G) \\
      first(\overline{\gamma}, G) & {\tt otherwise}
    \end{array} 
  \right . 
\end{array}
$$ 

Sometimes, for convenient we omit the second parameter $G$.


Given $null$, $first$ and $follow$ computed, we can construct a *predictive parsing table*

For each production rule $N ::= \overline{\sigma}$, we put the production rule in 

* cell $(N,t)$ if $t \in first(\overline{\sigma})$
* cell $(N,t')$ if $null(\overline{\sigma})$ and $t' \in follow(N)$


## Exercise 5

Check whether the following grammar is `LL(1)`.

```
S ::= AB
A ::= fe
A ::= epsilon
B ::= fg
```



## Parser Combinator without backtracking (In spirit of LL(1))

Let's extend our parser combinator to support `LL(1)` parsing without backtracking.

We introduce the following algebraic datatype to label an (intermediate) parsing result 
```scala
enum Progress[+A] {
    case Consumed(value: A)
    case Empty(value: A)
}
```

The partial is is `Consumed` when there has been input tokens consumed, otherwise, `Empty`.

We adjust the definition of the `Parser` case class as follows


```scala
case class Parser[T, A](p: List[T] => Progress[Result[(A, List[T])]]) {
    def map[B](f: A => B): Parser[T, B] = this match {
        case Parser(p) =>
            Parser(toks =>
                p(toks) match {
                    case Empty(Failed(err))    => Empty(Failed(err))
                    case Empty(Ok((a, toks1)))  => Empty(Ok((f(a), toks1)))
                    case Consumed(Failed(err)) => Consumed(Failed(err))
                    case Consumed(Ok((a, toks1))) =>
                        Consumed(Ok((f(a), toks1)))
                }
            )
    }
    def flatMap[B](f: A => Parser[T, B]): Parser[T, B] = this match {
        case Parser(p) =>
            Parser(toks =>
                p(toks) match {
                    case Consumed(v) => {
                        lazy val cont = v match {
                            case Failed(err) => Failed(err)
                            case Ok((a, toks1)) =>
                                f(a) match {
                                    case Parser(p2) =>
                                        p2(toks1) match {
                                            case Consumed(x) => x
                                            case Empty(x)    => x
                                        }
                                }
                        }
                        Consumed(cont)
                    }
                    case Empty(v) =>
                        v match {
                            case Failed(err) => Empty(Failed(err))
                            case Ok((a, toks1)) =>
                                f(a) match {
                                    case Parser(p2) => p2(toks1)
                                }
                        }
                }
            )
    }
}

def run[T, A](
    parser: Parser[T, A]
)(toks: List[T]): Progress[Result[(A, List[T])]] = parser match {
    case Parser(p) => p(toks)
}
```

Let's look at the `flatMap` which is the Monadic `bind` eventually. It takes the first parser (`this`) and pattern-matches it against `Parser(p)`. In the output parser, we first apply `p` to the tokens `toks`. 

* If the result's progress is `Empty(v)`, we check whether `v` is an error or `Ok`. When it is an error, it will be propogated, otherwise, we apply `f` to the output of `p(toks)` which is `a`. That will give us the second parser to continue with `Parser(p2)`. We then apply `p2` to `toks1` which should be the same as `toks`. 
* If the result's progress is `Consumed(v)`, we note that some part of `toks` has been parsed. The parser's behaviour here should be similar to the previous case, except that `p2(toks1)` progress result will always be updated as `Consumed` regardless whether `p2` has consumed anything. The lazy declaration of the immutable variable `cont` is an optimization which allows us to return the progress information `Consumed` without actually executing `p2(toks1)` when its result is not needed.


The defintion of the MonadError type class instance for Parser is as follows

```scala
type ParserM = [T] =>> [A] =>> Parser[T, A]

given parsecMonadError[T]: MonadError[ParserM[T], Error] =
    new MonadError[ParserM[T], Error] {
        override def pure[A](a: A): Parser[T, A] =
            Parser(cs => Empty(Ok((a, cs))))
        override def bind[A, B](
            fa: Parser[T, A]
        )(f: A => Parser[T, B]): Parser[T, B] = fa.flatMap(f)

        override def raiseError[A](e: Error): Parser[T, A] =
            Parser(toks => Empty(Failed(e)))
        override def handleErrorWith[A](
            fa: Parser[T, A]
        )(f: Error => Parser[T, A]): Parser[T, A] = fa match {
            case Parser(p) =>
                Parser(toks =>
                    p(toks) match {
                        case Empty(
                                Failed(err)
                            ) => // only backtrack when it is empty
                            {
                                f(err) match {
                                    case Parser(p2) => p2(toks)
                                }
                            }
                        case Empty(
                                Ok(v)
                            ) => // LL(1) parser will also attempt to look at f if fa does not consume anything
                            {
                                f("faked error") match {
                                    case Parser(p2) =>
                                        p2(toks) match {
                                            case Empty(_) =>
                                                Empty(
                                                    Ok(v)
                                                ) // if f also fail, we report the error from fa
                                            case consumed => consumed
                                        }
                                }
                            }
                        case Consumed(v) => Consumed(v)
                    }
                )
        }

    }
```

The interesting part lies in the `handleErrorWith` method, which the error recovery method `fa` is applied only when the progress of the parsing so far is `Empty`. In other words, given a choice of two parsers `choice(p1)(p2)`, it will not backtrack to `p2` if `p1` has consumed some token. This is in-sync with predictive parsing.

All other combinators such as `choice`, `item`, `sat`, `optional` can be adjusted in the same fashion.

We know that not all languages are in `LL(1)`.  It is undecidable to find out which `k` of `LL(k)` that a language is in. Thus, the above parser might not be very useful since it only works if the given language is in `LL(1)`. 

Thanks to the Monadic design, it is very easy to extend the parser to accept non `LL(1)` language by supporting backtracking on-demand. That is, the parser by default is not backtracking, however, it could if we want it to backtrack explicitly.

```scala 
// explicit try and backtrack if fails
def attempt[T, A](p: Parser[T, A]): Parser[T, A] =
    Parser(toks =>
        run(p)(toks) match {
            case Consumed(Failed(err)) => Empty(Failed(err))
            case otherwise => otherwise 
        }
    )
```
The `attempt` combinator takes a parser `p` and runs it. If its result is `Consumed` but `Failed`, it will *reset* the progress as `Empty`.  The full implementation of the on-demand backtracking parser combinator implementation is given in the stub project `Parsec.scala`.

## Optional Exercise 6

Re-implement the Math Expression parser and Json parser using `Parsec.scala`.

## Optional Exercise 7

Re-implement the Lexers using `Parsec.scala`.
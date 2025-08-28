# 50.054 - Static semantics 

# Learning Outcomes 

1. Apply type checking rules to verify the type correctness property of a SIMP program.
1. Apply type inference rules and unification to determine the most general type environment given a SIMP program
1. Apply type checking algorithm to type check a simply typed lambda calculus expression.


# Exercise 1 

Consider the following SIMP program, assuming

```python
x = input; 
s = 0;     
c = 0;     
while c < x {   
    s = c + s;  
    c = c + 1;  
}
return s;
```

1. Apply the type inference algorithm to infer the types of the above program.

# Exercise 2

We extend the syntax and typing rules for Simply typed lambda calculus from the notes with `<`

$$
\begin{array}{rccl}
 {\tt (Lambda\ Terms)} & t & ::= & x \mid \lambda x:T.t \mid t\ t \mid let\ x:T =\ t\ in\ t \mid  if\ t\ then\ t\ else\ t \mid t\ op\ t \mid c \mid fix\ t \\
 {\tt (Builtin\ Operators)} & op & ::= & + \mid - \mid * \mid / \mid\ == \mid < \\
 {\tt (Builtin\ Constants)} & c & ::= & 0 \mid 1 \mid ... \mid true \mid false \\
 {\tt (Types)} & T & ::= & int \mid bool \mid T \rightarrow T \\ 
 {\tt (Type\ Environments)} & \Gamma & \subseteq & (x \times T)
\end{array}
$$

And we extended the $(lctOp2)$ and $(lctOp3)$ rules as follows

$$
\begin{array}{cc}
{\tt (lctOp2)} & \begin{array}{c}
               \Gamma \vdash t_1 : int \ \ \ \Gamma \vdash t_2 : int \ op \in \{<, ==\}\\
               \hline
               \Gamma \vdash  t_1\ op t_2 : bool 
               \end{array} \\ \\ 
{\tt (lctOp3)} & \begin{array}{c}
               \Gamma \vdash t_1 : bool \ \ \ \Gamma \vdash t_2 : bool\ op \in \{<, ==\} \\
               \hline
               \Gamma \vdash  t_1\ op t_2 : bool 
               \end{array} \\ \\ 
\end{array}
$$

```haskell
fix \g:int->int->int.\x:int.\y:int.if x < y then g x (y-x) else (if x == y then x else g (x-y) y)
```

For simplicity, we write `\x:T.e` to denote $\lambda x:T.e$.

1. Apply the type checking algorithm to check the above program has type `int`


# Exercise 3

Complete the given code project which implements a type checker for the above specification of simply typed lambda calculus.
Here are the tasks

1. Study the codes given in `SimplyTypedLambdaCalculus.scala`. You should find the lambda calculus term implementation as a set of Scala enum types and the $fv()$ implementation.
    * Note that the let-bound variables and lambda-bound variables are having type annotation
    * An additional type is defined.
      1. `Type` - the simply typed lambda calculus type
1. Study the codes given in `Util.scala`. It contains the `StateResultMonadError` type class and its instances. As well as `appSubst`. We need them in the type checking implementation.
    * In last week's cohort problems, we defined them in `BigStepEval.scala`. In this cohort problem, since some of these library functions are needed by `TypeCheck.scala`, we moved them out to a separate module `Util.scala`.
1. Study the codes given in `TypeCheck.scala`. There is a main function `typeCheck` implemented (partially). Complete the missing parts in `typeCheck`
1. You should be able to test your code using `sbt test`


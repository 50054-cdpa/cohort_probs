# 50.054 - dynamic semantics 

# Learning Outcomes 

1. Explain the small step operational semantics of a programming language.
1. Explain the big step operational semantics of a programming language.
1. Formalize the run-time behavior of a programming language using small step operational semantics.
1. Formalize the run-time behavior of a programming language using big step operational semantics.


Recall that small step operational semantics is to formally define the run-time behavior of a program in a step by step manner.

Given the operational semantics as a specification, it is straight-forward to implement an interpreter. For instance the interpreter code for Pseudo Assembly is given in the SIMP project template.

# Exercise 1 

Consider the following SIMP program, assuming $input = 2$

```python
x = input;
f = 1;
s = 1;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
```

1. Apply the small step operational semantics to execute the above program.
1. Apply the big step operational semantics to execute the above program.


# Exercise 2

Recall that lambda calculus syntax.

$$
\begin{array}{rccl}
 {\tt (Lambda\ Terms)} & t & ::= & x \mid \lambda x.t \mid t\ t \mid if\ t\ then\ t\ else\ t \mid t\ op\ t \mid fix\ t \mid c \\
 {\tt (Builtin\ Operators)} & op & ::= & + \mid - \mid * \mid / \mid\ == \\
 {\tt (Builtin\ Constants)} & c & ::= & 0 \mid 1 \mid ... \mid true \mid false 
\end{array}
$$

We adjust the recursion support by replacing the $\mu$-abstraction with the $fix$ operator. The $fix$ operator is like the $Y$ combinator, except that $Y$ combinator is not a builtin operator, but defined by as a regular lambda abstraction term. $fix$ is a builtin operator. 

The small step operational semantics


$$
\begin{array}{rc}
{\tt (NOR)} & \begin{array}{c}
                t_1 \longrightarrow t_1' \\ 
                \hline
                t_1\ t_2 \longrightarrow t_1'\ t_2
                \end{array}  \\ \\
{\tt (\beta\ reduction)} & (\lambda x.t_1)\ t_2 \longrightarrow [t_2/x] t_1
\\ \\
{\tt (ifI)} & 
  \begin{array}{c} 
    t_1 \longrightarrow t_1'  \\
    \hline
    if\ t_1\ then\ t_2\ else\ t_3 \longrightarrow if\ t_1'\ then\ t_2\ else\ t_3 
  \end{array} \\  \\
{\tt (ifT)} &  if\ true\ then\ t_2\ else\ t_3 \longrightarrow t_2 \\ \\
{\tt (ifF)} &  if\ false\ then\ t_2\ else\ t_3 \longrightarrow t_3  \\ \\
{\tt (OpI1)} & \begin{array}{c} 
                t_1 \longrightarrow t_1' \\ 
                \hline 
                t_1\ op\ t_2\  \longrightarrow t_1'\ op\ t_2 
                \end{array} \\ \\
{\tt (OpI2)} & \begin{array}{c} 
                t_2 \longrightarrow t_2' \\ 
                \hline 
                c_1\ op\ t_2\  \longrightarrow c_1\ op\ t_2' 
                \end{array} \\ \\
{\tt (OpC)} &  \begin{array}{c} 
                invoke\ low\ level\ call\  op(c_1, c_2) = c_3 \\ 
                \hline  
                c_1\ op\ c_2\  \longrightarrow c_3 
                \end{array} \\ \\
{\tt (Let)} & let\ x=t_1\ in\ t_2 \longrightarrow [t_1/x]t_2 \\ \\
{\tt (Fix1)} & \begin{array}{c}
               t \longrightarrow t'\\
               \hline
               fix\ t \longrightarrow fix\ t'
               \end{array}  \\ \\ 
{\tt (Fix2)} &  fix\ \lambda f.t \longrightarrow \lbrack (fix\ \lambda f.t)/f \rbrack t
\end{array}
$$

Rule ${\tt (Fix1)}$ evaluates the argument of $fix$ operator by a step, until it becomes a lambda abstraction.
Rule ${\tt (Fix2)}$ "unfold" the fixed point function $\lambda f.t$, by subsituting occurences of $f$ in $t$ by $fix\ \lambda f.t$.

and free variable extrction function

$$
\begin{array}{rcl}
fv(x) & = & \{x\}\\
fv(\lambda x.t) & = & fv(t) - \{x\} \\ 
fv(t_1\ t_2) & = & fv(t_1) \cup fv(t_2)  \\ 
fv(let\ x=t_1\ in\ t_2) & = & (fv(t_1) - \{x\}) \cup fv(t_2) \\
fv(if\ t_1\ then\ t_2\ else\ t_3) & = & fv(t_1) \cup fv(t_2) \cup fv(t_3) \\
fv(c) & = & \{\} \\ 
fv(t_1\ op\ t_2) & = & fv(t_1) \cup fv(t_2) \\ 
fv(fix\ t) & = & fv(t)
\end{array}
$$

and substitution operation

$$
\begin{array}{rcll}
 \lbrack t_1 / x \rbrack c & = & c \\ 
 \lbrack t_1 / x \rbrack x & = & t_1 \\
 \lbrack t_1 / x \rbrack y & = & y & {\tt if}\  x \neq y \\
 \lbrack t_1 / x \rbrack (t_2\ t_3) & = & \lbrack t_1 / x \rbrack t_2\ 
 \lbrack t_1 / x \rbrack t_3 & \\
 \lbrack t_1 / x \rbrack \lambda y.t_2 & = & \lambda y. \lbrack t_1 / x
 \rbrack t_2 & {\tt if}\  y\neq x\  {\tt and}\  y \not \in fv(t_1) \\ 
 \lbrack t_1 / x \rbrack let\ y = t_2\ in\ t_3 & = & let\ y = \lbrack t_1 / x \rbrack t_2\ in\ \lbrack t_1 / x \rbrack t_3 & {\tt if}\  y\neq x\  {\tt and}\  y \not \in fv(t_1) \\ 
  \lbrack t_1 / x \rbrack if\ t_2\ then\ t_3\ else\ t_4 & = & if\ \lbrack t_1 / x \rbrack t_2\ then\ \lbrack t_1 / x \rbrack t_3\ else\ \lbrack t_1 / x \rbrack t_4 \\ 
  \lbrack t_1 / x \rbrack t_2\ op\ t_3 & = & (\lbrack t_1 / x \rbrack t_2)\ op\ (\lbrack t_1 / x \rbrack t_3) \\ 
  \lbrack t_1 / x \rbrack (fix\ t_2) & = & fix\ \lbrack t_1 / x \rbrack t_2 &  
\end{array}
$$

To define the big step operational semantics for the above language, $t \Downarrow v$, we need to define what is a value $v$ in our language. 

$$
\begin{array}{rcll}
{(\tt Value)} & v & ::= c \mid \lambda x.t
\end{array}
$$

which states that a value in our lambda calculus is either a constant or a lambda abstraction.


Your task is to complete the following set of big step operational semantics rules for our lambda calculus.


$$
\begin{array}{rc}
{\tt (lcbFix)} & \begin{array}{c}
              t \Downarrow \lambda f.t' \ \ \ \lbrack (fix\ \lambda f.t')/f \rbrack t' \Downarrow v
              \\ \hline
              fix\ t \Downarrow v
              \end{array} \\ \\
{\tt (lcbApp)} & \begin{array}{c}
              t_1 \Downarrow \lambda x.t_3 \ \ \  \lbrack t_2/x\rbrack t_3 \Downarrow v
              \\ \hline 
              t_1\ t_2 \Downarrow v
              \end{array} \\ \\
{\tt (lcbIf1)} & \begin{array}{c}
                 t_1 \Downarrow true \ \ \ t_2 \Downarrow v
                 \\ \hline
                 if\ t_1\ then\ t_2\ else\ t_3 \Downarrow v
                 \end{array} \\ \\
\end{array}
$$

Note that rules ${\tt (lcbIf2)}$, ${\tt (lcbLet)}$, ${\tt (lcbOp)}$ are missing.


# Exercise 3

Complete the given code project which implements an intepretor for the above specification of lambda calculus.
Here are the tasks

1. Study the codes given in `LambdaCalculus.scala`. You should find the lambda calculus term implementation as a set of Scala enum types and the $fv()$ implementation.
    * Types defined.
      1. `Term` - the lambda calculus terms according to the grammar rule
      1. `Var`  - the variables in lambda calculus
      1. `Const` - the constants in lambda calclus
      1. `Op` - the binary operators
      1. `Value` - the values in the big step operational semantics
1. Study the codes given in `BigStepEval.scala`. There are two main functions implemented (partially), a set of type definitions and type class instances
    * Types defined.
        1. `Subst` - a tuple type represent a substitution
        1. `Err` - the error message
        1. `StateInfo` - the state objects keep track of the fresh variable generation.
        1. `Result` - the result of the evaluation and subtitution, just like `Option`
        1. `StateResult[A]` - a type alias to the state transition objects `StateT[StateInfo, Result, A]`
    * Type classes defined.
        1. `StateResultMonadError[S]` - a derived trait of `StateTMonadError[S, Result, Err]`
            * `StateTMonadError[S,M[_],E]` is a monad transformer combining state monad, with another container monad `M`, defined in `StateT.scala`
            * `StateTMonadError[S, Result, Err]` is a monad generated by combining a state monad with a `Result` monad inside. 
    * Type class instances defined.
        1. `resultMonadError` - a type class instance declaring that `Result` is an instance of the `MonadError` type class.
        1. `stateResultMonadError` - a type class instance of `StateResultMonadError[S]`
    * Functions
        1. `get` and `put` - getting and setting the state of the state monad
        1. `newName` - generates a new name from the state monad
        1. `appSubst` - applies a substitution to a lambda term $[t/x]t'$. It deviates from the design above in the following ways.
            * In case of variable clashing arising in let-binding and function application, instead of failing the substitution, we apply alpha-renaming (You don't need to worry about these parts, they have been included in the code given).
            * That is the reason why we need a state monad to keep track of the state, i.e. the current running number for variable name generation
        1. `eval` - evaluates a lambda term to a value. It deviates from the design above in the following ways.
            * In case of variable term (the free one), an error is returned.
            * In case of type errors, i.e. $fix$ is applied to a non-lambda, function application with a non-lambda, if-condition is non-boolean, etc., errors will be returned.
            * In case of a value term, it is retured.
            * That's the reason why need a monad error type class.
        1. `equal`, `plus`, `minus`, ... are the helper functions to support `eval`.
1. Complete the missing parts in `appSubst` 
1. Complete the missing parts in `eval`
1. You should be able to test your code using `sbt test`



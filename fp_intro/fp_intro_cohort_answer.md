% Instroduction to functional programming

# Learning Outcomes
By the end of this class, you should be able to 

* Comprehend, evaluate lambda terms
* Differentiate different evaluation strategies
* Apply lambda calculus to implement simple algorithms

# Lambda Calculus

## Lambda Expression


The valid syntax of lambda expression is described as the following EBNF

$$
\begin{array}{rccl}
 {\tt (Lambda\ Terms)} & t & ::= & x \mid \lambda x.t \mid t\ t
\end{array}
$$

Where 

* $x$ denotes a variable, 
* $\lambda x.t$ denotes a lambda abstraction. 
  * Within a lambda abstraction,  $x$ is the bound variable (c.f. formal argument of the function) 
and $t$ is the body.
* $t\ t$ denotes a function application.


Note that given a lambda term, there might be multiple ways of parsing (interpreting) it. For instance, 
Given $\lambda x.x\ \lambda y.y$, we could interpret it as either

1. $(\lambda x.x)\ (\lambda y.y)$, or
2. $\lambda x.(x\ \lambda y.y)$


# Exercise 1

Consider the lambda term $\lambda x.x\ \lambda y.y$ which can be parsed in multiple ways. By putting paratheses (...), show that this lambda term can be parsed in at least 2 different ways.

Answer

$(\lambda x.x)\ (\lambda y.y)$ and $\lambda x.(x\ \lambda y.y)$.


## Free variables


$$
\begin{array}{rcl}
fv(x) & = & \{x\}\\
fv(\lambda x.t) & = & fv(t) - \{x\} \\ 
fv(t_1\ t_2) & = & fv(t_1) \cup fv(t_2) 
\end{array}
$$


# Exercise 2 

Assuming function application has a higher precedence level than lambda
abstraction, i.e. $\lambda x.t_1\ t_2$ is always parsed as $\lambda x.(t_1\ t_2)$, function application
is left associative, i.e. $t_1\ t_2\ t_3$ is always parsed as $(t_1\ t_2)\ t_3$ , compute the
free variables from the following lambda terms.
* $\lambda x.x\ y$
* $\lambda x.x\ (\lambda y.x\ y)\ y$
* $\lambda x.(\lambda y.(\lambda z.x\ z\ z)\ y)\ y$


Answer:
* $fv(\lambda x.x\ y) = fv(\lambda x.(x\ y)) = fv(x\ y) - \{x\} = \{x, y\} - \{x\} = \{y\}$
* 
$$
\begin{array}{ll}
fv(\lambda x.x\ (\lambda y.x\ y)\ y) & = \\ 
fv(\lambda x.(x\ (\lambda y.x\ y)\ y)) & = \\ 
fv(x\ (\lambda y.x\ y)\ y) - \{x\} & = \\
 (\{x \} \cup fv(\lambda y.x\ y) \cup \{y\}) - \{x\} & = \\ 
 (\{x\} \cup (fv(x\ y) - \{y\}) \cup \{y\}) - \{x\} & = \\ 
 (\{x\} \cup (\{x,y\} - \{y\}) \cup \{y\}) - \{x\} & = \\ 
 (\{x\} \cup \{x\} \cup \{y\}) - \{x\} & = \\ 
 \{y\}
 \end{array}
$$
* 
$$
\begin{array} {l}
fv(\lambda x.(\lambda y.(\lambda z.x\ z\ z)\ y)\ y) = \\
fv(\lambda x.((\lambda y.((\lambda z.(x\ z\ z))\ y))\ y)) = \\
fv((\lambda y.((\lambda z.(x\ z\ z))\ y))\ y) - \{x \} = \\ 
(fv(\lambda y.((\lambda z.(x\ z\ z))\ y)) \cup \{y\}) - \{x \} = \\ 
((fv((\lambda z.(x\ z\ z))\ y) - \{y\}) \cup \{y\}) - \{x \} = \\ 
(((fv(\lambda z.(x\ z\ z))\cup\{y\}) - \{y\}) \cup \{y\}) - \{x \} = \\ 
(((((fv(x\ z\ z)-\{z\}))\cup\{y\}) - \{y\}) \cup \{y\}) - \{x \} = \\ 
(((\{x\}\cup\{y\}) - \{y\}) \cup \{y\}) - \{x \} = \\ 
\{y\}
\end{array}
$$

### Beta Reduction

$$
\begin{array}{rl}
{\tt (\beta\ reduction)} & (\lambda x.t_1)\ t_2 \longrightarrow [t_2/x] t_1
\end{array}
$$

where  $t \longrightarrow t'$ denotes one evaluation step. $[t_2/x]$
refers to a substitution. $[t_2/x]t_1$ denotes the application of the
substitution $[t_2/x]$ to $t_1$, Informally speaking it means we
replace every occurance of the formal argument $x$ in $t_1$ with $t_2$. 

### Substitution and Alpha Renaming


$$
\begin{array}{rcll}
 \lbrack t_1 / x \rbrack x & = & t_1 \\
 \lbrack t_1 / x \rbrack y & = & y & {\tt if}\  x \neq y \\
 \lbrack t_1 / x \rbrack (t_2\ t_3) & = & \lbrack t_1 / x \rbrack t_2\ 
 \lbrack t_1 / x \rbrack t_3 & \\
 \lbrack t_1 / x \rbrack \lambda y.t_2 & = & \lambda y. \lbrack t_1 / x
 \rbrack t_2 & {\tt if}\  y\neq x\  {\tt and}\  y \not \in fv(t_1)
\end{array}
$$


In case  

$$
y\neq x\  {\tt and} \ y \not \in fv(t_1)
$$ 

is not satified, we need to rename the lambda bound variables that are clashing.

# Exercise 3

Apply the substitution $[(y\ \lambda x.x)/x]$ to the following lambda terms, high-light the variables in name clashing or free variables being mis-captured if there exists; otherwise, derive the result.
* $\lambda y.y\ x$
* $\lambda z.x\ z$
* $(\lambda z.z)\ y$
* $(\lambda x.x)\ z$

Answer:

* $[(y\ \lambda x.x)/x](\lambda y.y\ x) = [(y\ \lambda x.x)/x](\lambda y.(y\ x))$.
Clash! Because lambda bound variable $y \in fv(y\ \lambda x.x)$.

* $[(y\ \lambda x.x)/x](\lambda z.x\ z)  = [(y\ \lambda x.x)/x](\lambda z.(x\ z)) = \lambda z. [(y\ \lambda x.x)/x](x\ z) = \lambda z. ((y\ \lambda x.x)\ z)$

* $[(y\ \lambda x.x)/x] ((\lambda z.z)\ y) = [(y\ \lambda x.x)/x] (\lambda z.z)\ [(y\ \lambda x.x)/x]y = (\lambda z.([(y\ \lambda x.x)/x]z))\ y = (\lambda z.z)\ y$

* $[(y\ \lambda x.x)/x] ((\lambda x.x)\ z) = [(y\ \lambda x.x)/x](\lambda x.x)\ [(y\ \lambda x.x)/x]z$. Clash! Because lambda bound variable $x$ is in the domain of the substitution $[(y\ \lambda x.x)/x]$.

## Evaluation strategies


1. Inner-most, leftmost - Applicative Order Reduction
2. Outer-most, leftmost - Normal Order Reduction


# Exercise 4

Apply AOR and NOR to evaluate the following lambda term.


* $(\lambda x.x)\ (\lambda y.y)\ (\lambda z.z)$
* $\lambda x.(\lambda x.x\ x\ y)\ (\lambda z.z\ x)$
* $(\lambda x.x\ x)\ (\lambda x.x\ x)$
* $(\lambda y.\lambda x.y)\ (\lambda z.z)\ ((\lambda x.x\ x)\ (\lambda x.x\ x))$


Answer

* $(\lambda x.x)\ (\lambda y.y)\ (\lambda z.z) = ((\lambda x.x)\ (\lambda y.y))\ (\lambda z.z)$
  * AOR 
  
```math
    \begin{array}{ll}
    \underline{((\lambda x.x)\ (\lambda y.y))} & \longrightarrow_{\beta} \\ 
    ([\lambda y.y/x]\ x)\ (\lambda z. z) & \longrightarrow_{subs} \\ 
    \underline{(\lambda y.y)\ (\lambda z.z)} & \longrightarrow_{\beta} \\ 
    [\lambda z.z/y] y & \longrightarrow_{subs} \\ 
    \lambda z.z
    \end{array}
```
    
  * NOR - same as AOR
* $\lambda x.(\lambda x.x\ x\ y)\ (\lambda z.z\ x) = \lambda x.((\lambda x.((x\ x)\ y))\ (\lambda z.(z\ x)))$
  * AOR
  
```math
  \begin{array}{ll}
  \lambda x.\underline{((\lambda x.((x\ x)\ y))\ (\lambda z.(z\ x)))} & \longrightarrow_\beta \\
  \lambda x.([(\lambda z.(z\ x))/x]((x\ x)\ y)) & \longrightarrow_{subs} \\ 
  \lambda x.(\underline{((\lambda z.(z\ x))\ (\lambda z.(z\ x)))}\ y) & \longrightarrow_\beta \\ 
  \lambda x.(([\lambda z.(z\ x)/z](z\ x))\ y) & \longrightarrow_{subs} \\ 
  \lambda x.(\underline{((\lambda z.(z\ x))\ x)}\ y) & \longrightarrow_{\beta} \\ 
  \lambda x.(([x/z](z\ x))\ y) & \longrightarrow_{subs} \\ 
  \lambda x.((x\ x)\ y)
  \end{array}
```
  
  * NOR - same as AOR
* $(\lambda x.x\ x)\ (\lambda x.x\ x) = (\lambda x.(x\ x))\ (\lambda x.(x\ x))$
  * AOR - diverges
    
```math
    \begin{array}{ll}
    \underline{(\lambda x.(x\ x))\ (\lambda x.(x\ x))}&  \longrightarrow_\beta \\ 
    [\lambda x.(x\ x)/x](x\ x) & \longrightarrow_{subs} \\ 
    \underline{(\lambda x.(x\ x))\ (\lambda x.(x\ x))}&  \longrightarrow_\beta \\ 
    ...
     \end{array}
```

  * NOR - same as AOR

* $(\lambda y.\lambda x.y)\ (\lambda z.z)\ ((\lambda x.x\ x)\ (\lambda x.x\ x))= ((\lambda y.(\lambda x.y))\ (\lambda z.z))\ ((\lambda x.(x\ x))\ (\lambda x.(x\ x)))$
  * AOR - diverges
   
```math
    \begin{array}{ll}
    \underline{((\lambda y.(\lambda x.y))\ (\lambda z.z))}\ ((\lambda x.(x\ x))\ (\lambda x.(x\ x))) & \longrightarrow_\beta \\ 
    ([\lambda z.z/y](\lambda x.y))\ ((\lambda x.(x\ x))\ (\lambda x.(x\ x))) & \longrightarrow_{subs} \\ 
    (\lambda x.(\lambda z.z))\ \underline{((\lambda x.(x\ x))\ (\lambda x.(x\ x)))} & \longrightarrow_{\beta} \\ 
    ...
    \end{array}
```

   * NOR - terminates

```math
    \begin{array}{ll}
    \underline{((\lambda y.(\lambda x.y))\ (\lambda z.z))}\ ((\lambda x.(x\ x))\ (\lambda x.(x\ x))) & \longrightarrow_\beta \\ 
    ([\lambda z.z/y](\lambda x.y))\ ((\lambda x.(x\ x))\ (\lambda x.(x\ x))) & \longrightarrow_{subs} \\ 
    \underline{(\lambda x.(\lambda z.z))\ ((\lambda x.(x\ x))\ (\lambda x.(x\ x)))} & \longrightarrow_{\beta} \\ 
    [((\lambda x.(x\ x))\ (\lambda x.(x\ x)))/x](\lambda z.z) & \longrightarrow_{subs} \\ 
    \lambda z.z
    \end{array}
```


## Lambda Calculus Extended 



$$
\begin{array}{rccl}
 {\tt (Lambda\ Terms)} & t & ::= & x \mid \lambda x.t \mid t\ t \mid if\ t\ then\ t\ else\ t \mid t\ op\ t \mid \mu f.t \mid c \\
 {\tt (Builtin\ Operators)} & op & ::= & + \mid - \mid * \mid / \mid\ == \\
 {\tt (Builtin\ Constants)} & c & ::= & 0 \mid 1 \mid ... \mid true \mid false 
\end{array}
$$

and the evaluation rules

$$
\begin{array}{rc}
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
{\tt (NOR)} & \begin{array}{c}
                t_1 \longrightarrow t_1' \\ 
                \hline
                t_1\ t_2 \longrightarrow t_1'\ t_2
                \end{array}  \\ \\
{\tt (unfold)} & \mu f.t \longrightarrow [(\mu f.t)/f] t 
\end{array}
$$

and free variable extended

$$
\begin{array}{rcl}
fv(x) & = & \{x\}\\
fv(\lambda x.t) & = & fv(t) - \{x\} \\ 
fv(t_1\ t_2) & = & fv(t_1) \cup fv(t_2)  \\ 
fv(let\ x=t_1\ in\ t_2) & = & (fv(t_1) - \{x\}) \cup fv(t_2) \\
fv(if\ t_1\ then\ t_2\ else\ t_3) & = & fv(t_1) \cup fv(t_2) \cup fv(t_3) \\
fv(c) & = & \{\} \\ 
fv(\mu f.t) & = & fv(t) - \{f\} 
\end{array}
$$

and substitution extended


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
  \lbrack t_1 / x \rbrack \mu f.t_2 & = & \mu f.\lbrack t_1 / x \rbrack t_2 & {\tt if}\  f\neq x\  {\tt and}\  f \not \in fv(t_1) 
\end{array}
$$

# Exercise 5

Evaluate the following lambda term

$$
let\ fac = \mu F.\lambda x. if\ x == 0\ then\ 1\ else\ (x*(F\ (x-1)))\ in (fac\ 3) 
$$

Answer: incomplete

$$
\begin{array}{ll}
let\ fac = \mu F.\lambda x. if\ x == 0\ then\ 1\ else\ (x*(F\ (x-1)))\ in (fac\ 3) & \longrightarrow_{\tt (Let)} \\
[\mu F.\lambda x. if\ x == 0\ then\ 1\ else\ (x*(F\ (x-1)))/fac](fac\ 3)  & \longrightarrow_{subs} \\ 
\mu F.\lambda x. if\ x == 0\ then\ 1\ else\ (x*(F\ (x-1)))\ 3 & \longrightarrow_{\tt (unfold)} \\ 
\underline{\lambda x. if\ x == 0\ then\ 1\ else\ (x*((\mu F.\lambda x. if\ x == 0\ then\ 1\ else\ (x*(F\ (x-1))))\ (x-1)))\ 3} & \longrightarrow_\beta \\
[3/x](if\ x == 0\ then\ 1\ else\ (x*((\mu F.\lambda x. if\ x == 0\ then\ 1\ else\ (x*(F\ (x-1))))\ (x-1)))) & \longrightarrow_{subs+\alpha} \\ 
(if\ 3 == 0\ then\ 1\ else\ (3*((\mu F.\lambda y. if\ y == 0\ then\ 1\ else\ (y*(F\ (y-1))))\ (3-1)))) & \longrightarrow_{\tt (IfI, OpC, IfF)} \\ 
(3*((\mu F.\lambda y. if\ y == 0\ then\ 1\ else\ (y*(F\ (y-1))))\ 2)) & \longrightarrow_{\tt (OpI2, unfold)} \\
(3*\underline{(\lambda y.if\ y == 0\ then\ 1\ else\ (y*((\mu F.\lambda y. if\ y == 0\ then\ 1\ else\ (y*(F\ (y-1))))\ (y-1)))\ 2)}) & \longrightarrow_\beta \\ 
...
\end{array}
$$

# Exercise 6

The above set of evaluation rules does not force actual arguments of function application to being evaluated first before being applying to the function body. This is known as the strict evaluation. What changes is required if we make to enforce strict evaluation

Answer:

Firstly we include value in the syntax.

$$
\begin{array}{rccl}
 {\tt (Values)} & v & ::= & \lambda x.t \mid c
\end{array}
$$

We replace the ${\tt (\beta\ reduction)} $ and ${\tt (NOR)} $ rules the following evaluation rules

$$
\begin{array}{rl}
{\tt (App1)} & \begin{array}{c}
                t_1 \longrightarrow t_1' \\ 
                \hline
                t_1\ t_2 \longrightarrow t_1'\ t_2
                \end{array}  \\ \\
{\tt (App2)} & \begin{array}{c}
                t_2 \longrightarrow t_2' \\ 
                \hline
                v_1\ t_2 \longrightarrow v_1\ t_2
                \end{array}  \\ \\
{\tt (\beta\ reduction)} & (\lambda x.t_1)\ v_2 \longrightarrow [v_2/x] t_1
\end{array}
$$


# Optional Exercise - Church Encoding


Recall that Y-combinator is defined as

$$
Y := \lambda f.(\lambda y.(f\ (y\ y))\ (\lambda x.(f\ (x\ x)))
$$

## Exercise 7
Show that for any function $g$, we have $Y\ g = g\ (Y\ g)$.



Let’s encode natural numbers. The main idea is to define

$$
\begin{array}{l}
0 := \lambda f.\lambda x.x \\ 
1 := \lambda f.\lambda x.f\ x \\
2 := \lambda f.\lambda x.f\ (f\ x) \\ 
3 := \lambda f.\lambda x.f\ (f\ (f\ x)) \\
... := \\ 
... \\
\end{array}
$$

## succ (AKA incr) operation

It can be defined as

$$
succ := \lambda n.\lambda f.\lambda x.f\ (n\ f\ x)
$$

The idea is that we apply one more $f$ to the input number. The
input number can be accessed by applying $n$ to $f$ and $x$.

## Exercise 8
Test $succ$ by evaluating $succ\ 0$ to show that it equals to $1$.

##  pred (AKA decr) operation

$$
pred := \lambda n.\lambda f.\lambda x.n\ (\lambda g.\lambda h.h\ (g\ f ))\ (\lambda u.x)\ (\lambda u.u)
$$

## Exercise 9

Test $pred$ by evaluating $pred\ 3$.


Recall that in Church encoding $true$ and $false$ are defined as

$$
\begin{array}{l}
true:= \lambda x.\lambda y.x \\
false:= \lambda x.\lambda y.y
\end{array}
$$

The $izero$ function is rather simple.

$$
iszero := \lambda n.n\ (\lambda x.false)\ true
$$

## Exercise 10
Test $izero$ with $iszero\ 0$ and $iszero\ 1$



## Factorial 

Recall the $fac$ function implementation,

$$
fac := Y\ Fac
$$

where

$$
Fac := \lambda fac.\lambda n.ite\ (iszero\ n)\ one\ (mul\ n\ (fac\ (pred\ n)))
$$

and

$$
mult := \lambda m.\lambda n.\lambda f.m\ (n\ f)
$$

## Exercise 11
Try evaluating $fac\ 3$.


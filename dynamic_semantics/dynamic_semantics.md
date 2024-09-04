# 50.054 - dynamic semantics 

# Learning Outcomes 

1. Explain the small step operational semantics of a programming language.
1. Explain the big step operational semantics of a programming language.
1. Formalize the run-time behavior of a programming language using small step operational semantics.
1. Formalize the run-time behavior of a programming language using big step operational semantics.


Recall that small step operational semantics is to formally define the run-time behavior of a program in a step by step manner.

Given the operational semantics as a specification, it is straight-forward to implement an interpretor. For instance the interpretor code for SIMP is given in the SIMP project template.

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

## Answer

1. small step 
```python
{(input,2)}, 
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
---> # using (sSeq)
   {(input,2)}, x = input
   ---> # (sAssign1)
   {(input,2)}, x = 2
   ---> # (sAssign2)
   {(input,2), (x,2)}, nop
--->
{(input,2), (x,2)},
nop
f = 1;
s = 1;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # using (sNopSeq)
{(input,2), (x,2)},
f = 1;
s = 1;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # using (sSeq)
  {(input,2), (x,2)},f = 1;
   ---> # (sAssign2)
   {(input,2), (x,2), (f,1)}, nop;
--->
{(input,2), (x,2), (f,1)},
nop;
s = 1;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # using (sNopSeq)
{(input,2), (x,2), (f,1)},
s = 1;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # using (sSeq) 
  {(input,2), (x,2), (f,1)}, s = 1;
  ---> # (sAssign2)
  {(input,2), (x,2), (f,1), (s,1)}, nop;
--->
{(input,2), (x,2), (f,1), (s,1)},
nop;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # using (sNopSeq)
{(input,2), (x,2), (f,1), (s,1)},
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # using (sSeq)
  {(input,2), (x,2), (f,1), (s,1)}, t=0;
  ---> # (sAssign2)
  {(input,2), (x,2), (f,1), (s,1), (t,0)}, nop;
--->
{(input,2), (x,2), (f,1), (s,1), (t,0)},
nop;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # (sNopSeq)
{(input,2), (x,2), (f,1), (s,1), (t,0)},
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s;
---> # (sSeq)
  {(input,2), (x,2), (f,1), (s,1), (t,0)}, 
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sWhile)
  {(input,2), (x,2), (f,1), (s,1), (t,0)}, 
  if s < x {
    t = f;
    f = s;  
    s = t + f;
    while s < x {
      t = f;
      f = s;
      s = t + f;
    } 
  else {
    nop;
  }
  ---> # (sIf1)
    {(input,2), (x,2), (f,1), (s,1), (t,0)}, s < x
    ---> # (sOp1)
    {(input,2), (x,2), (f,1), (s,1), (t,0)}, 1 < x
    ---> # (sOp2)
    {(input,2), (x,2), (f,1), (s,1), (t,0)}, 1 < 2
    ---> # (sOp3)
    {(input,2), (x,2), (f,1), (s,1), (t,0)}, true
  --->
  {(input,2), (x,2), (f,1), (s,1), (t,0)}
  if true {
    t = f;
    f = s;  
    s = t + f;
    while s < x {
      t = f;
      f = s;
      s = t + f;
    } 
  else {
    nop;
  }
  ---> # (sIf2)
  {(input,2), (x,2), (f,1), (s,1), (t,0)}
  t = f;
  f = s;  
  s = t + f;
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sSeq)
    {(input,2), (x,2), (f,1), (s,1), (t,0)}, t = f;
    ---> # (sAssign1)
    {(input,2), (x,2), (f,1), (s,1), (t,0)}, t = 1;
    ---> # (sAssign2)
    {(input,2), (x,2), (f,1), (s,1), (t,1)}, nop;
  ---> 
  {(input,2), (x,2), (f,1), (s,1), (t,1)}
  nop;
  f = s;  
  s = t + f;
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sNopSeq)
  {(input,2), (x,2), (f,1), (s,1), (t,1)}
  f = s;  
  s = t + f;
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sSeq)
    {(input,2), (x,2), (f,1), (s,1), (t,1)}, f = s;
    ---> # (sAssign1)
    {(input,2), (x,2), (f,1), (s,1), (t,1)}, f = 1;
    ---> # (sAssign2)
    {(input,2), (x,2), (f,1), (s,1), (t,1)}, nop;
  ---> 
  {(input,2), (x,2), (f,1), (s,1), (t,1)},
  nop; 
  s = t + f;
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sNopSeq)
  {(input,2), (x,2), (f,1), (s,1), (t,1)},
  s = t + f;
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sSeq)
    {(input,2), (x,2), (f,1), (s,1), (t,1)}, s = t + f;
    ---> # (sAssign1)
      {(input,2), (x,2), (f,1), (s,1), (t,1)}, t + f
      ---> # (sOp1) 
      {(input,2), (x,2), (f,1), (s,1), (t,1)}, 1 + f
      ---> # (sOp2) 
      {(input,2), (x,2), (f,1), (s,1), (t,1)}, 1 + 1
      ---> # (sOp3) 
      {(input,2), (x,2), (f,1), (s,1), (t,1)}, 2
    ---> 
    {(input,2), (x,2), (f,1), (s,1), (t,1)}, s = 2;
    ---> # (sAssign2)
    {(input,2), (x,2), (f,1), (s,2), (t,1)}, nop;
  ---> 
  {(input,2), (x,2), (f,1), (s,2), (t,1)}, 
  nop;
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sNopSeq)
  {(input,2), (x,2), (f,1), (s,2), (t,1)}, 
  while s < x {
    t = f;
    f = s;
    s = t + f;
  }
  ---> # (sWhile)
  {(input,2), (x,2), (f,1), (s,2), (t,1)}, 
  if s < x {
    t = f;
    f = s;
    s = t + f;
    x = x + 1;
    while s < x {
      t = f;
      f = s;
      s = t + f;
      x = x + 1;
    }
  } else {
    nop;
  }
  ---> # (sIf1)
    {(input,2), (x,2), (f,1), (s,2), (t,1)}, s < x
    ---> # (sOp1)
    {(input,2), (x,2), (f,1), (s,2), (t,1)}, 2 < x
    ---> # (sOp1)
    {(input,2), (x,2), (f,1), (s,2), (t,1)}, 2 < 2
    ---> # (sOp3)
    {(input,2), (x,2), (f,1), (s,2), (t,1)}, false
  ---> 
  if false {
    t = f;
    f = s;
    s = t + f;
    x = x + 1;
    while s < x {
      t = f;
      f = s;
      s = t + f;
      x = x + 1;
    }
  } else {
    nop;
  }
  ---> # (sIf3)
  {(input,2), (x,2), (f,1), (s,2), (t,1)},
  nop;
--->
{(input,2), (x,2), (f,1), (s,2), (t,1)},
nop;
return s;
---> # (sNopSeq)
{(input,2), (x,2), (f,1), (s,2), (t,1)},
return s;
```

2. big step
```python
                                   {(input,2), (x,2)} |- 1 ⇓ 1 (bConst)
                                   ---------------------(bAssign)     [sub tree 1]
                                   {(input,2), (x,2)}, 
                                   f = 1; 
{(input,2)} |- input ⇓ 2 (bVar)    ⇓ {(input,2), (x,2), (f,1)}      
---------------- (bAssign)         -------------------------------------------(bSeq)
{(input,2)},                       {(input,2), (x,2)},  
x = input;                         f = 1;
⇓ {(input,2), (x,2)}               s = 1;
                                   t = 0;
                                   while s < x {
                                   t = f;
                                   f = s;
                                   s = t + f;
                                   }
                                   return s; ⇓ {(input,2),(x,2),(f,1),(s,2),(t,1)}
---------------------------------------------------------------------------- (bSeq)
{(input, 2)}, 
x = input;
f = 1;
s = 1;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s; ⇓ {(input,2),(x,2),(f,1),(s,2),(t,1)}
```

Where [sub tree 1] is 

```python
                                      {(input,2),(x,2),(f,1),(s,1)}
                                      |- 0 ⇓ 0    (bConst)
                                      --------------------------------  [sub tree 2]
{(input,2), (x,2), (f,1)}             {(input,2),(x,2),(f,1),(s,1)}
|- 1 ⇓ 1 (bConst)                     t = 0; ⇓
                                      {(input,2),(x,2),(f,1),(s,1),(t,0)}
--------------------------(bAssign)   -------------------------------------- (bSeq)
{(input,2), (x,2), (f,1)},            {(input,2), (x,2), (f,1), (s,1)}, 
s = 1;                                t = 0; 
⇓                                     while s < x {t = f; f = s; s = t + f;} 
{(input,2), (x,2), (f,1), (s,1)},     return s; ⇓{(input,2),(x,2),(f,1),(s,2),(t,1)}
---------------------------------------------------------------------------- (bSeq)
{(input,2), (x,2), (f,1)}, 
s = 1;
t = 0;
while s < x {
  t = f;
  f = s;
  s = t + f;
}
return s; ⇓ {(input,2),(x,2),(f,1),(s,2),(t,1)}
```

Where [sub tree 2] is 

```python 
                --------------------------------------------------- (bReturn)
[sub tree 3]    {(input,2),(x,2),(f,1),(s,2),(t,1)}, return s; ⇓
                {(input,2),(x,2),(f,1),(s,2),(t,1)}, 
---------------------------------------------------------------------------- (bSeq)
{(input,2),(x,2),(f,1),(s,1),(t,0)}, 
while s < x {t = f; f = s; s = t + f;}
return s; ⇓ {(input,2),(x,2),(f,1),(s,2),(t,1)}
```


Where [sub tree 3] is


```python

{(input,2),(x,2),(f,1),(s,1),(t,0)}
|- s ⇓ 1 (bVar)

{(input,2),(x,2),(f,1),(s,1),(t,0)}
|- x ⇓ 2 (bVar)

1 < s == true                               [sub tree 4]          [sub tree 5]
------------------------------------ (bOp) ----------------------------------- (bSeq)
{(input,2),(x,2),(f,1),(s,1),(t,0)}        {(input,2),(x,2),(f,1),(s,1),(t,0)}, 
|- s < x ⇓ true                             t = f; f = s; s = t + f;
                                            while s < x {t = f; f = s; s = t + f;} ⇓
                                            {(input,2),(x,2),(f,1),(s,2),(t,1)}
---------------------------------------------------------------------------- (bWhile)
{(input,2),(x,2),(f,1),(s,1),(t,0)}, 
while s < x {t = f; f = s; s = t + f;} ⇓  
{(input,2),(x,2),(f,1),(s,2),(t,1)}
```

Where [sub tree 4] is 

```python

{(input,2),(x,2),(f,1),(s,1),(t,0)} |-   (bVar)
f ⇓ 1  
---------------------------------------------- (bAssign)
{(input,2),(x,2),(f,1),(s,1),(t,0)}, 
t = f; ⇓ {(input,2),(x,2),(f,1),(s,1),(t,1)}
```

Where [sub tree 5] is 
```python

{(input,2),(x,2),(f,1),(s,1),(t,1)} |- (bVar)
s ⇓ 1
---------------------------------------- (bAssign)      [sub tree 6]
{(input,2),(x,2),(f,1),(s,1),(t,1)},
f = s; ⇓ {(input,2),(x,2),(f,1),(s,1),(t,1)}
--------------------------------------------------------------------------(bSeq)
{(input,2),(x,2),(f,1),(s,1),(t,1)}, 
f = s; s = t + f; 
while s < x {t = f; f = s; s = t + f;} ⇓
{(input,2),(x,2),(f,1),(s,2),(t,1)}
```


Where [sub tree 6] is 

```python
{(input,2),(x,2),(f,1),(s,1),(t,1)} |- (bVar)
t ⇓ 1 
{(input,2),(x,2),(f,1),(s,1),(t,1)} |- (bVar)
f ⇓ 1 

t + f == 2
-------------------------------------------
{(input,2),(x,2),(f,1),(s,1),(t,1)} |- (bOp)
t + f ⇓ 2
---------------------------------------- (bAssign)      [sub tree 7]
 {(input,2),(x,2),(f,1),(s,1),(t,1)}
s = t + f;  ⇓ {(input,2),(x,2),(f,1),(s,2),(t,1)}
------------------------------------------------------------------------(bSeq)
{(input,2),(x,2),(f,1),(s,1),(t,1)}, 
s = t + f; 
while s < x {t = f; f = s; s = t + f;} ⇓ 
{(input,2),(x,2),(f,1),(s,2),(t,1)}
```

Where [sub tree 7] is 

```python
{(input,2),(x,2),(f,1),(s,2),(t,1)} |- (bVar)
s ⇓ 2

{(input,2),(x,2),(f,1),(s,2),(t,1)} |- (bVar)
x ⇓ 2

s < x == false
-------------------------------------------(bOp)
{(input,2),(x,2),(f,1),(s,2),(t,1)} |- 
s < x ⇓ false
------------------------------------------------------------ (bWhile)
{(input,2),(x,2),(f,1),(s,2),(t,1)},
while s < x {t = f; f = s; s = t + f;} ⇓ 
{(input,2),(x,2),(f,1),(s,2),(t,1)}
```

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

## Answer
$$
\begin{array}{rc}
{\tt (lcbIf2)} & \begin{array}{c}
                 t_1 \Downarrow false \ \ \ t_3 \Downarrow v
                 \\ \hline
                 if\ t_1\ then\ t_2\ else\ t_3 \Downarrow v
                 \end{array} \\ \\ 
{\tt (lcbLet)} & \begin{array}{c}
                 \lbrack t_1/x \rbrack t_2 \Downarrow v
                 \\ \hline
                 let\ x = t_1\ in\ t_2 \Downarrow v
                 \end{array} \\ \\
{\tt (lcbOp)} & \begin{array}{c}
                t_1 \Downarrow v_1 \ \ \ t_2 \Downarrow v_2\ \ \ op(v_1, v_2) = v_3
                \\ \hline 
                t_1\ op\ t_2 \Downarrow v_3
                \end{array}
\end{array}
$$

# Exercise 3

Complete the given code project which implements an intepretor for the above specification of lambda calculus.
Here are the tasks

1. Study the codes given in `LambdaCalculus.hs`. You should find the lambda calculus term implementation as a set of Haskell data types and the $fv()$ implementation.
    * Types defined.
      1. `Term` - the lambda calculus terms according to the grammar rule
      1. `Var`  - the variables in lambda calculus
      1. `Const` - the constants in lambda calclus
      1. `Op` - the binary operators
      1. `Value` - the values in the big step operational semantics
1. Study the codes given in `BigStepEval.hs`. There are two main functions implemented (partially), a set of type definitions and type class instances
    * Types defined.
        1. `Subst` - a tuple type represent a substitution
        1. `Err` - the error message
        1. `StateInfo` - the state objects keep track of the fresh variable generation.
        1. `Result` - the result of the evaluation and subtitution, just like `Option`
        1. `StateResult s a` - a type alias to the state transformer objects `StateT s Result a`
    * Type class instances defined.
        1.  a type class instance declaring that `Result` is an instance of the `MonadError` type class.
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
1. You should be able to test your code using `cabal test` or `stack test`


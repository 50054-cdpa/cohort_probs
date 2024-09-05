# 50.054 - syntax analysis


## Learning Outcome 

By the end of this class, you should be able to 

* Implement a simple lexer using regex
* Implement a top-down recursive parser with backtracking
* Implement a top-down recursive parser with on-demand backtracking 


## Exercise 1  

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

```hs
data LToken = IntTok Int | PlusTok | AsterixTok 
    deriving (Show, Eq)
```

Following the same idea mentioned in the lecture note, we make use of the regex library to implement a lexer.

```hs
integer = "^([[:digit:]]+)(.*)"
plus  = "^(\\+)(.*)"
asterix = "^(\\*)(.*)"

type Error = String
```
Implement a lexer of MathExp using Haskell regex library. 
There is some skeleton code given in the project stub. Your task is to complete the missing parts and make sure it passes the test cases.


## Exercise 2

Eliminate the left-recursion of the following grammar.

```
X ::= XXa | Y
Y ::= Yb | c
```



## Recap LL(1) Parsing

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


## Exercise 3

Check whether the following grammar is `LL(1)`.

```
S ::= AB
A ::= fe
A ::= epsilon
B ::= fg
```



## Exercise 4

Recall the JSON syntax grammar

```
J  ::= i | 's' | [] | [IS] | {NS}
IS ::= J,IS | J
NS ::= N,NS | N
N ::= 's':J
```

Implement a parser of JSON. Is the grammar suitable for top-down parsing? 

There is some skeleton code given in the project stub `JsonParser.hs`.

Note that we use the following Haskell data type to represent the abstract syntax of JSON language. 

```hs
import Data.Map 

data Json = 
    IntLit Int | 
    StrLit String | 
    JsonList [Json] |
    JsonObject (Map String Json)
    deriving (Show, Eq)
```

Where `Map` (not to be confused with `map`) is a data structure defined in the library `containers`. It defines a mapping from key to values, (like Pythong dictionary). In the above we use `Map String Json` to represent JSON objects like `{"k1": 1, "k2": {}}`. 
For detailed operations defined over `Map`, you may refer to 
```url
https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
```
Your task is to complete the missing parts and make sure it passes the test cases.



## Exercise 5

Re-implement the Math Expression parser and Json parser using `Parsec.hs`.

## Optional Exercise 6

Re-implement the Lexers using `Parsec.hs`.

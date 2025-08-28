# 50.054 Name Analysis

## Learning Outcomes

1. Articulate the purpose of name analysis.
1. Describe the properties of the static single assignment forms.
1. Implement the static single assignment construction and deconstruction algorithms.


## Exercise 1

Construct the control flow graph of the following PA program. We construct one vertex per instruction.

```java
// PA_Ex1
1: x <- input
2: r <- 0
3: i <- 0
4: b <- i < x
5: ifn b goto 20
6: f <- 0
7: s <- 1
8: j <- 0
9: t <- 0
10: b <- j < i
11: ifn b goto 17
12: t <- f
13: f <- s
14: s <- t + f
15: j <- j + 1
16: goto 10
17: r <- r + s
18: i <- i + 1
19: goto 4
20: _ret_r <- r
21: ret
```
## Exercise 2 

Based on the result from the previous exercise, draw the dominace tree of `PA_Ex1`.

## Exercise 3

Based on the result from the previous exercises, construct the dominance frontier table of `PA_Ex1`.



## Exercise 4

Based on the result from the previous exercises, derived the minimal SSA of the  `PA_Ex1`


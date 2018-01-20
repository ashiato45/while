# while
## What is this?
An implementation of usual WHILE language.

## Usage
Currently it works as a library and does not work as a standalone application.
Open `stack ghci` in the directory and use the run.
For example:
```
*Main Lib> let ast = test programParser "x=100;y=0;WHILE[(x>0)]{y=(y+x);x=(x-1)}"
*Main Lib> fmap (calcProgram empty) ast
Right (False,fromList [("x",0 % 1),("y",5050 % 1)])

```

## Syntax
```
eop ::= + | - | * | /
exp ::= integer | variable | (exp eop exp)
bop ::= & | |
comp ::= == | < | <= | > | >= | !=
bool ::= TRUE | FALSE | (bool bop bool) | !bop | (exp comp exp)
prog ::= EXIT | ERROR | var = exp | prog;prog | WHILE[bool]{prog} | IF[bool]{prog} | IF[bool]{prog}ELSE{prog}
```
Alphabets "abcdefghijklmnopqrstuvwxyz'_" can be used as a name of a variable.

## References
- https://kunigami.blog/2014/01/21/an-introduction-to-the-parsec-library/
- http://gihyo.jp/dev/feature/01/functional-prog/0005

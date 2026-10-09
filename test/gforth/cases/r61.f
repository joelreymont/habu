\ A local declared in an if arm is gone after then (checked).
: P ( n -- n ) dup 0 > if {: a :} a 1 + else 1 - then a ;
5 P . cr

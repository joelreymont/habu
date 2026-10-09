\ A local declared in an if arm is no name in the else arm (checked).
: P ( n -- n ) dup 0 > if {: a :} a 1 + else a 1 - then ;
5 P . cr

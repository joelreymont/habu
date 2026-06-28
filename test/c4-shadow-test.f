\ c4-shadow-test.f - C4: locals may shadow ordinary words/builtins in a body
\ (dup/over/code, and i/j/k outside a do-loop). The structural case (a local
\ shadowing the ACTIVE loop index inside a do-loop) is B2's domain (rejected).
: C4DUP  ( n -- n )   {: dup :}  dup ;
: C4CODE ( n n -- n ) {: a code :} code a + ;
: C4OVER ( n -- n )   {: over :} over ;
: C4RUN ( -- ) 5 C4DUP .  3 4 C4CODE .  8 C4OVER . ;
C4RUN
s" c4-shadow ok (dup/code/over usable as locals)" type cr

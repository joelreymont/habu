\ A definer whose head and clause both compile calls. Its clause is checked
\ first; Gforth compiles the head, does>, then the clause, and a checked body
\ calls the created word.
: MK ( n -- ) 1 + create , does> ( -- n ) @ dup + ;
5 MK X
: USE ( -- n ) X 1 + ;
X . USE . cr

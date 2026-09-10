\ cell-effects.f - effects for words needed before the checker starts.

s" CELL" s" -- n" TRUST
s" CELL-WIDTH-CHECK" s" --" TRUST
s" CHECKER-CAPTURE-PREPARE" s" --" TRUST

s" PTR-VARIABLE" s" --" TRUST
s" PERSISTED-PTR-VARIABLE" s" --" TRUST
s" NULL-PTR" s" -- ptr a" TRUST
s" LBUF-CAPTURE-PREPARE" s" --" TRUST

\ These two cells are declared by variable before checking starts. Export
\ their concrete storage type so source-verifier accessors are checked.
s" MULTI-ERR" s" -- ptr n" TRUST
s" SIG-RAW-MODE" s" -- ptr n" TRUST

\ Read finalized numeric call facts without exposing the unification graph.
package CHECKER-CALLS

create STATE 0 , 0 , 0 ,

: INSTALL ( -- ) [: STATE ;] is CWIN-STATE ;
INSTALL

: FIELD ( n n -- ptr n ) {: row:n field:n :}
   STATE 0 ptr-field @ row 4 * field + cells + ;

: FIND ( n n -- n n ) {: ord:n kind:n :}
   STATE CELL + @ 0 ?do
      i 0 FIELD @ ord = i 3 FIELD @ kind = and if
         i 1 FIELD @ i 2 FIELD @ unloop exit
      then
   loop
   -1 -1 ;

get-current prot-wid-add
public

: CELLS ( n -- n n ) 3 FIND ;
: GLUE ( n -- n n ) 4 FIND ;
: QUOT-IN ( n n -- n n ) 2 * 5 + FIND ;
: QUOT-OUT ( n n -- n n ) 2 * 6 + FIND ;

get-current prot-wid-add
;package

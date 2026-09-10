\ cell-effects.f - effects for words needed before the checker starts.

s" CELL" s" -- n" TRUST
s" CELL-WIDTH-CHECK" s" --" TRUST
s" CHECKER-CAPTURE-PREPARE" s" --" TRUST

s" PTR-VARIABLE" s" --" TRUST
s" PERSISTED-PTR-VARIABLE" s" --" TRUST
s" NULL-PTR" s" -- ptr a" TRUST
s" LBUF-CAPTURE-PREPARE" s" --" TRUST

\ These variable accessors are emitted before checking starts. Their concrete
\ storage effects let the multi-error operations below check normally.
PRIM: MULTI-ERR PE-PTR-N PE-OUT PRIM;
PRIM: MULTI-ERR-N PE-PTR-N PE-OUT PRIM;
PRIM: MEO-ON PE-PTR-N PE-OUT PRIM;

s" SIG-RAW-MODE" s" -- ptr n" TRUST

\ Enter a fresh collection; END returns its reject count and clears the mode.
: MULTI-ERR-BEGIN ( -- )
   -1 MULTI-ERR !  0 MULTI-ERR-N !  0 MEO-ON ! ;


: MULTI-ERR-END ( -- n )
   MULTI-ERR-N @  0 MULTI-ERR !  0 MEO-ON ! ;


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
: MATCH-PAYLOAD ( n -- n n ) -3 FIND ;
: QUOT-IN ( n n -- n n ) 2 * 5 + FIND ;
: QUOT-OUT ( n n -- n n ) 2 * 6 + FIND ;

get-current prot-wid-add
;package

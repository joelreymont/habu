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
\ The floor is read from this source owner's private effect arena.
TRUSTED: MULTI-ERR-BEGIN ( -- )
   UEND @ CHECKER-EFFECT-AUTHORITY:RECOVERY-START
   -1 MULTI-ERR !  0 MULTI-ERR-N !  0 MEO-ON ! ;


: MULTI-ERR-END ( -- n )
   MULTI-ERR-N @  0 MULTI-ERR !  0 MEO-ON ! ;


\ Read finalized numeric call facts without exposing the unification graph.
package CHECKER-CALLS

\ The header's first cell holds the row arena's address, so it is DECLARED
\ storage and the two counts are allotted behind it (dot
\ habu-refuse-a-ptr-5ad2734e). A `create`d cell is raw storage: fetching an
\ address out of one is the launder the checker refuses, and `0 ptr-field` on it
\ was exactly that. The counts are numbers, read through an explicit cell view of
\ the declared head - the same record, the same compiled add-and-load, since both
\ views are type-level only.
PTR-VARIABLE STATE 0 , 0 ,

\ The owner can record calls before this checked header is loaded. Transfer
\ that allocation with its rows, then clear the retired header so capture has
\ only one live store to prepare. Reinstalling the current header is harmless.
: INSTALL ( -- )
   CWIN-STATE {: prior:ptr :}
   prior STATE = if exit then
   prior @ STATE !
   prior CELL + BYTE-VIEW CELL-VIEW @ STATE CELL + BYTE-VIEW CELL-VIEW !
   prior 2 CELL * + BYTE-VIEW CELL-VIEW @ STATE 2 CELL * + BYTE-VIEW CELL-VIEW !
   NULL-PTR prior !
   0 prior CELL + BYTE-VIEW CELL-VIEW !
   0 prior 2 CELL * + BYTE-VIEW CELL-VIEW !
   [: STATE ;] is CWIN-STATE ;
INSTALL

: FIELD ( n n -- ptr n ) {: row:n field:n :}
   STATE @ row 4 * field + CELL * + CELL-VIEW ;

: FIND ( n n -- n n ) {: ord:n kind:n :}
   STATE CELL + BYTE-VIEW CELL-VIEW @ 0 ?do
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

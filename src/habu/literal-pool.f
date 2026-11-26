\ literal-pool.f - LITERAL-POOL, the data a unit's code names, held back until
\ the unit's code ends: each arch's DATA-AFTER, lays it there.
require src/core/exit-status.f
require src/core/rejection.f
require src/lib/arena.f
require src/lib/vector.f
package LITERAL-POOL
private

\ A datum: its label, as LABEL>N gives it, and the bytes laid there.
STRUCTURE datum DERIVE addr
   FIELD lbl n
   FIELD text str
;STRUCTURE

\ A file link zeroes scratch cells after each piece, so the list is made when
\ first needed, in an arena: a cross-build runs this file on its host.
TYPED-VARIABLE POOL-ARENA ^ARENA:arena SCRATCH-ONLY
TYPED-VARIABLE POOL-VEC ^VEC:vector<datum> SCRATCH-ONLY

: POOL ( -- ^VEC:vector<datum> )
   POOL-VEC @ NULL-PTR = if
      ARENA:NEW POOL-ARENA !
      POOL-ARENA @ ARENA:ALLOCATOR 0 [type] datum VEC:NEW POOL-VEC !
   then
   POOL-VEC @ ;

public

\ Holds a copy of the bytes for the label numbered n.
: ADD ( n str -- ) {: lbl:n text:str :}
   POOL VEC:PUSH {: d:^ :}
   lbl d DATUM-LBL !  text POOL-ARENA @ ARENA:COPY d DATUM-TEXT ! ;

: COUNT ( -- n ) POOL VEC:COUNT ;

: AT ( n -- n str ) {: k:n :}
   POOL k VEC:AT {: d:^ :}
   d DATUM-LBL @  d DATUM-TEXT @ ;

: EMPTY ( -- )
   POOL-VEC @ NULL-PTR = if exit then
   POOL-VEC @ 0 VEC:RESIZE ;

\ When a build's code ends, every datum DATA-AFTER named has been laid.
: ?LAID ( -- )
   POOL {: list:^ :}
   list VEC:COUNT 0= if exit then
   s" E-DATA-UNLAID" REJECTION:OPEN
   s" the code ended with data DATA-AFTER named still waiting to be laid"
   REJECTION:EMIT-STR
   s" waiting" REJECTION:NOTE  list VEC:COUNT REJECTION:EMIT-U
   s" first" REJECTION:NOTE  list 0 VEC:AT DATUM-TEXT @ REJECTION:EMIT-STR
   REJECTION:HELP
   s" end the code unit that names the data with DATA-AFTER," REJECTION:EMIT-STR
   EXIT-STATUS:ENCODE REJECTION:STOP ;

;package

\ A stripped application that hands qsort a callback. A binding is process
\ state, which no image carries (lib/task.f drops every row at capture), so the
\ entry word binds its two slots, sorts and unbinds. The outer comparator sorts
\ a second row through the inner one, so the image nests a foreign call inside
\ a callback at the tier an application is linked at.
require lib/ffi-abi.f
require lib/ffi-callback.f
require lib/task.f

package STRIPPED-CALLBACK-SUBJECT
private

$4A constant FAILURE-RC
8 constant ROW-N

PROCESS-SYMBOLS

FUNCTION: QSORT qsort ( ptr u8 n n n -- )
   0 $40 WRITES-BYTES                \ ROW-N cells
;FUNCTION

create OUTER ROW-N cells allot
create INNER ROW-N cells allot
variable INNER-FN
variable NESTED

CALLBACK: CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-NEST ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

: ROW-FILL ( ptr n n -- ) {: row:ptr base:n :}
   ROW-N 0 ?do base ROW-N + i - row i cells + ! loop ;

: ROW-SORTED? ( ptr n n -- bool ) {: row:ptr base:n :}
   true ROW-N 0 ?do row i cells + @ base 1 + i + = and loop ;

: SIGN-OF ( n n -- n ) {: a:n b:n :}
   a b < if -1 exit then
   a b > if 1 exit then
   0 ;

: CMP-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ SIGN-OF ;

: CMP-NEST-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   INNER 100 ROW-FILL
   INNER BYTE-VIEW ROW-N CELL INNER-FN @ QSORT
   INNER 100 ROW-SORTED? if 1 NESTED +! then
   a b CMP-IMPL ;

' CMP-IMPL CMP-BODY !
' CMP-NEST-IMPL CMP-NEST-BODY !

public

: RUN ( -- )
   0 NESTED !
   CMP TASK:SELF-CONTEXT FFI-CB:ENTRY INNER-FN !
   CMP-NEST TASK:SELF-CONTEXT FFI-CB:ENTRY {: outer:n :}
   OUTER 10 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL outer QSORT
   CMP-NEST FFI-CB:UNBIND
   CMP FFI-CB:UNBIND
   OUTER 10 ROW-SORTED? 0= if
      s" stripped-callback: the row is not sorted" FAILURE-RC die
   then
   NESTED @ 0= if
      s" stripped-callback: no nested sort came back sorted" FAILURE-RC die
   then
   s" stripped-callback: ok" type cr ;

;package

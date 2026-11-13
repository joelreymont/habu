\ ffi-callback-fixture.f - what src/lib/ffi-callback-test.f and the child it
\ spawns (test/ffi-callback-child.f) share: libc's qsort, bsearch and pthread
\ entry points, the callbacks C is handed, and the cells their bodies report
\ through. Each file reopens package FFI-CB-TEST.
\ Every row sorted here is ROW-N cells, which is the extent QSORT declares
\ written.
require src/lib/error.f
require src/lib/string.f
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f
require src/lib/task.f
require src/lib/adt/result.f

package FFI-CB-TEST
using FFI-CB

8 constant ROW-N
-9971 constant E-BOOM                \ the throwing comparator's code, outside every lib block
$5EA1 constant SENTINEL-A
$5EB2 constant SENTINEL-B
$1F constant SORTS-ALL               \ SORTS with every case true

PROCESS-SYMBOLS

FUNCTION: QSORT qsort ( ptr u8 n n n -- )
   0 $40 WRITES-BYTES                \ ROW-N cells
;FUNCTION

\ The matched element's address as a number, 0 for none.
FUNCTION: BSEARCH bsearch ( ptr u8 ptr u8 n n n -- n ) ;FUNCTION

FUNCTION: PTHREAD-CREATE pthread_create ( ptr u8 n n n -- i32 )
   0 8 WRITES-BYTES                  \ pthread_t
;FUNCTION

create OUTER ROW-N cells allot        \ the row the calling task sorts
create INNER ROW-N cells allot        \ the row a comparator sorts, nested
create THREAD-ROW ROW-N cells allot   \ the row a second thread sorts

variable CMP-FN                       \ the C entry each ENTRY answered
variable CMP-SELF-FN
variable CMP-VIA-FN
variable CMP-BOOM-FN
variable CMP-UNBIND-FN
variable NESTED                       \ sorts a comparator ran
variable BAD                          \ nested sorts that came back unsorted
variable SELF-DEPTH                   \ 1 inside CMP-SELF's own nested sort
variable BOOMS
variable BOOM-ON
variable UNBIND-CODE                  \ what UNBIND threw inside the comparator
variable INSIDE                       \ 1 once a body is parked
variable GATE                         \ the release a parked body waits for
variable THREAD                       \ pthread_t
variable WHO-OWNER                    \ CB-OWNER of the region WHO last ran on
variable CMP-CALLER-FN
variable CALLER-WANT                  \ sorters a first comparison waits for
variable CALLER-INSIDE                \ sorters that reached CMP-CALLER
variable CALLER-MET                   \ sorters that saw CALLER-WANT inside
variable CALLER-MISSES                \ comparisons on another sorter's region
variable CALLER-UNBIND-ON             \ non-zero: a comparison tries UNBIND
variable CALLER-UNBIND-CODE
create CALLER-REGIONS 3 cells allot   \ each sorter's region, by hundreds
create CALLER-ENTERED 3 cells allot   \ 1 once that sorter reached CMP-CALLER
create ROW-A ROW-N cells allot
create ROW-B ROW-N cells allot

TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B

\ High to low, so a sort has every pair to order: base+ROW-N .. base+1.
: ROW-FILL ( ptr n n -- ) {: row:ptr base:n :}
   ROW-N 0 ?do base ROW-N + i - row i cells + ! loop ;

: ROW-SORTED? ( ptr n n -- bool ) {: row:ptr base:n :}
   true ROW-N 0 ?do row i cells + @ base 1 + i + = and loop ;

: ROW-SUM ( ptr n -- n ) {: row:ptr :}
   0 ROW-N 0 ?do row i cells + @ + loop ;

: SIGN-OF ( n n -- n ) {: a:n b:n :}
   a b < if -1 exit then
   a b > if 1 exit then
   0 ;

: ROW-ORDER ( ptr u8 ptr u8 -- n ) {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ SIGN-OF ;

CALLBACK: CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-SELF ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-VIA ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-BOOM ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-UNBIND ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: START ( n -- n ) 0 FALLBACK ;CALLBACK
\ The longest dispatch a declaration can generate, never called: no C caller
\ checks i32/u32 narrowing, float arguments, an r result or a full n result.
: DECLARE-MARSHAL ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" CALLBACK: MARSHAL ( n i32 u32 ptr u8 i32 u32 r r r r r r r r -- r ) 0.5 FFALLBACK ;CALLBACK" evaluate-closed
   else
      s" CALLBACK: MARSHAL ( n i32 u32 ptr u8 i32 u32 i32 u32 r r r r r r r r -- r ) 0.5 FFALLBACK ;CALLBACK" evaluate-closed
   then ;
DECLARE-MARSHAL
CALLBACK: UNSET ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: UNSET-ONE ( ptr u8 ptr u8 -- i32 ) 1 FALLBACK ;CALLBACK
CALLBACK: WHO ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-CALLER ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
10000 constant CALLER-WAIT-MS

: NEST ( n -- ) {: fn:n :}
   INNER 100 ROW-FILL
   INNER BYTE-VIEW ROW-N CELL fn QSORT
   INNER 100 ROW-SORTED? 0= if 1 BAD atomic-add drop then
   1 NESTED atomic-add drop ;

\ Nests through its own slot: the inner sort's comparisons arrive on the row
\ this call already holds.
: CMP-SELF-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   SELF-DEPTH @ 0= if
      1 SELF-DEPTH !
      CMP-SELF-FN @ NEST
      0 SELF-DEPTH !
   then
   a b ROW-ORDER ;

\ Nests through a second slot bound to the same context.
: CMP-VIA-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   CMP-FN @ NEST
   a b ROW-ORDER ;

: BOOM ( -- )
   1 BOOMS atomic-add drop
   BOOM-ON @ 0 <> if E-BOOM throw then ;

: CMP-BOOM-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   BOOM
   a b ROW-ORDER ;

: CMP-UNBIND-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   [: CMP-UNBIND UNBIND ;] catch UNBIND-CODE !
   a b ROW-ORDER ;

\ Holds a callback body until the owner opens GATE or halts the task it runs
\ as. It pauses before it looks, so a body that parks here once a halt is
\ pending always reaches a TASK:PAUSE inside the callback with the request set.
: PARK ( -- )
   1 INSIDE atomic!
   begin
      TASK:PAUSE
      GATE atomic@ 0<> TASK:HALTED? or
   until ;

\ Bound and never reached: a thread C started is refused before its dispatch.
: START-IMPL ( n -- n ) ;

\ A comparator that keeps the owner of the region it runs on, which is the
\ calling thread's.
: WHO-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   data-base CB-OWNER + atomic@ WHO-OWNER !
   a b ROW-ORDER ;


: CALLER-ENTER ( n -- ) {: band:n :}
   CALLER-ENTERED band cells + @ 0<> if exit then
   1 CALLER-ENTERED band cells + !
   1 CALLER-INSIDE atomic-add drop
   0
   begin
      dup CALLER-WAIT-MS <
      CALLER-INSIDE atomic@ CALLER-WANT @ < and
   while
      1 >MS TASK:SLEEP 1 +
   repeat
   drop
   CALLER-INSIDE atomic@ CALLER-WANT @ >= if 1 CALLER-MET atomic-add drop then ;


\ For a slot on TASK:CALLER-CONTEXT: a value's hundreds name the sorter whose
\ row it is, and the call must run on that sorter's region.
: CMP-CALLER-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   a CELL-VIEW @ 100 / {: band:n :}
   CALLER-REGIONS band cells + @ data-base FFI:>CELL <> if
      1 CALLER-MISSES atomic-add drop
   then
   band CALLER-ENTER
   CALLER-UNBIND-ON @ 0<> if
      0 CALLER-UNBIND-ON !
      [: CMP-CALLER UNBIND ;] catch CALLER-UNBIND-CODE !
   then
   a b ROW-ORDER ;

' ROW-ORDER CMP-BODY !
' CMP-SELF-IMPL CMP-SELF-BODY !
' CMP-VIA-IMPL CMP-VIA-BODY !
' CMP-BOOM-IMPL CMP-BOOM-BODY !
' CMP-UNBIND-IMPL CMP-UNBIND-BODY !
' START-IMPL START-BODY !
' WHO-IMPL WHO-BODY !
' CMP-CALLER-IMPL CMP-CALLER-BODY !
\ UNSET-BODY and UNSET-ONE-BODY are left unset: each dispatch has to refuse, not
\ execute the zero.

\ ---- bindings -----------------------------------------------------------------
\ The five comparators on the calling task's own context: the main region on
\ the main task, a worker's region on a worker.
: BIND-SELF ( -- )
   CMP TASK:SELF-CONTEXT ENTRY CMP-FN !
   CMP-SELF TASK:SELF-CONTEXT ENTRY CMP-SELF-FN !
   CMP-VIA TASK:SELF-CONTEXT ENTRY CMP-VIA-FN !
   CMP-BOOM TASK:SELF-CONTEXT ENTRY CMP-BOOM-FN !
   CMP-UNBIND TASK:SELF-CONTEXT ENTRY CMP-UNBIND-FN ! ;

: UNBIND-SELF ( -- )
   CMP UNBIND
   CMP-SELF UNBIND
   CMP-VIA UNBIND
   CMP-BOOM UNBIND
   CMP-UNBIND UNBIND ;

\ ---- the sorts a bound task runs ----------------------------------------------
\ Three sorts under one local, one loop index and one stack cell, each live
\ across the foreign call.
: SORT-PLAIN ( -- bool )
   SENTINEL-A {: keep:n :}
   true
   3 0 ?do
      OUTER i ROW-FILL
      SENTINEL-B
      OUTER BYTE-VIEW ROW-N CELL CMP-FN @ QSORT
      SENTINEL-B = and
      OUTER i ROW-SORTED? and
   loop
   keep SENTINEL-A = and ;

: SORT-NESTED ( n -- bool ) {: fn:n :}
   0 NESTED !  0 BAD !  0 SELF-DEPTH !
   OUTER 20 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL fn QSORT
   OUTER 20 ROW-SORTED?
   NESTED @ 0 > and
   BAD @ 0= and ;

\ Every comparison throws: C is answered the fallback, the code is kept, and
\ the stack under the call is where it was.
: SORT-BOOM ( -- bool )
   0 BOOMS !  1 BOOM-ON !
   CMP-BOOM CLEAR
   OUTER 10 ROW-FILL
   depth {: before:n :}
   OUTER BYTE-VIEW ROW-N CELL CMP-BOOM-FN @ QSORT
   depth before =
   0 BOOM-ON !
   CMP-BOOM FAULT@ E-BOOM = and
   BOOMS @ 0 > and
   OUTER ROW-SUM 116 = and
   CMP-BOOM CLEAR
   CMP-BOOM FAULT@ 0= and ;

\ The comparator tries to unbind the slot it arrived through.
: SORT-UNBIND ( -- bool )
   0 UNBIND-CODE !
   OUTER 30 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL CMP-UNBIND-FN @ QSORT
   OUTER 30 ROW-SORTED?
   UNBIND-CODE @ E-TASK-STATE = and ;

: MASK-BIT ( bool n -- n ) {: ok:bool b:n :}
   ok if b else 0 then ;

\ One bit per case, so a worker hands all five back through TASK:RETURN.
: SORTS ( -- n )
   SORT-PLAIN 1 MASK-BIT
   CMP-SELF-FN @ SORT-NESTED 2 MASK-BIT or
   CMP-VIA-FN @ SORT-NESTED 4 MASK-BIT or
   SORT-BOOM 8 MASK-BIT or
   SORT-UNBIND $10 MASK-BIT or ;

\ ---- a thread C starts --------------------------------------------------------
\ pthread_create's own answer: 0, or its errno.
: THREAD-START ( n n -- n ) {: fn:n arg:n :}
   THREAD BYTE-VIEW 0 fn arg PTHREAD-CREATE ;

: WAIT-INSIDE ( -- )
   begin INSIDE atomic@ 0= while TASK:PAUSE repeat ;

\ ---- worker bodies -------------------------------------------------------------
: JOINED ( result<n,n> -- n )
   MATCH result
      ok OF ENDOF
      err OF drop -1 ENDOF
   ;MATCH ;

: WORK-UNBINDS ( -- )
   BIND-SELF SORTS UNBIND-SELF TASK:RETURN ;

\ Ends with its five slots bound: the task's end drops them.
: WORK-ENDS ( -- )
   BIND-SELF SORTS TASK:RETURN ;

: CALLER-SORT ( ptr n n -- bool ) {: row:ptr band:n :}
   data-base FFI:>CELL CALLER-REGIONS band cells + !
   row band 100 * ROW-FILL
   row BYTE-VIEW ROW-N CELL CMP-CALLER-FN @ QSORT
   row band 100 * ROW-SORTED? ;

: WORK-CALLER-A ( -- ) ROW-A 1 CALLER-SORT 1 MASK-BIT TASK:RETURN ;
: WORK-CALLER-B ( -- ) ROW-B 2 CALLER-SORT 1 MASK-BIT TASK:RETURN ;

;using
;package

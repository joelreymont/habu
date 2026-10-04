\ ffi-callback-fixture.f - what lib/ffi-callback-test.f and the children it
\ spawns (test/ffi-callback-child.f) share: libc's qsort and pthread entry
\ points, the callbacks C is handed, and the cells their bodies report through.
\ Both files reopen package FFI-CB-TEST. Every row sorted here is ROW-N cells,
\ which is the extent QSORT declares written.
require lib/errors.f
require lib/string.f
require lib/ffi-abi.f
require lib/ffi-callback.f
require lib/task.f
require lib/adt/result.f

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

FUNCTION: PTHREAD-CREATE pthread_create ( ptr u8 n n n -- i32 )
   0 8 WRITES-BYTES                  \ pthread_t
;FUNCTION

FUNCTION: PTHREAD-JOIN pthread_join ( n ptr u8 -- i32 )
   1 8 WRITES-BYTES                  \ the start routine's void *
;FUNCTION

FUNCTION: PTHREAD-SELF pthread_self ( -- n ) ;FUNCTION

create OUTER ROW-N cells allot        \ the row the calling task sorts
create INNER ROW-N cells allot        \ the row a comparator sorts, nested
create THREAD-ROW ROW-N cells allot   \ the row a foreign thread sorts
create MARSHAL-BYTE $5A c,
align

variable CMP-FN                       \ the C entry each ENTRY answered
variable CMP-SELF-FN
variable CMP-VIA-FN
variable CMP-BOOM-FN
variable CMP-UNBIND-FN
variable START-FN
variable NESTED                       \ sorts a comparator ran
variable BAD                          \ nested sorts that came back unsorted
variable SELF-DEPTH                   \ 1 inside CMP-SELF's own nested sort
variable BOOMS
variable BOOM-ON
variable UNBIND-CODE                  \ what UNBIND threw inside the comparator
variable INSIDE                       \ 1 once a start routine is parked in its callback
variable GATE                         \ the release a parked routine waits for
variable HOLD                         \ non-zero keeps a released routine sorting
variable THREAD-SORTS                 \ sorts the routine has finished
variable THREAD                       \ pthread_t
variable THREAD-RET                   \ the start routine's result, from pthread_join
variable THREAD-OWNER                 \ CB-OWNER of the region the routine ran on
variable THREAD-SELF                  \ pthread_self as the routine read it
8 TYPED-BUFFER SEEN-INTS n            \ MARSHAL's arguments as its body received them
8 TYPED-BUFFER SEEN-FLOATS r

TASK:MIN-STACK TASK:TASK CTX-TASK     \ the region a foreign thread enters
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

\ The two cells C's comparator arguments point at.
: ROW-ORDER ( ptr u8 ptr u8 -- n ) {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ SIGN-OF ;

CALLBACK: CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-SELF ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-VIA ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-BOOM ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: CMP-UNBIND ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
CALLBACK: START ( n -- n ) 0 FALLBACK ;CALLBACK
CALLBACK: START-B ( n -- n ) 0 FALLBACK ;CALLBACK
\ Every integer and float argument register: AAPCS64 has eight integer slots,
\ while SysV x86-64 has six. Keep each declaration at its target's real limit.
: DECLARE-MARSHAL ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" CALLBACK: MARSHAL ( n i32 u32 ptr u8 i32 u32 r r r r r r r r -- r ) 0.5 FFALLBACK ;CALLBACK" evaluate-closed
   else
      s" CALLBACK: MARSHAL ( n i32 u32 ptr u8 i32 u32 i32 u32 r r r r r r r r -- r ) 0.5 FFALLBACK ;CALLBACK" evaluate-closed
   then ;
DECLARE-MARSHAL
CALLBACK: UNSET ( n -- n ) 7 FALLBACK ;CALLBACK
CALLBACK: WHO ( -- n ) 0 FALLBACK ;CALLBACK

\ INNER sorted through the entry fn, from inside a comparator.
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

\ A pthread start routine: it reports who it is, parks until the owner lets it
\ go, sorts a row of its own through CMP - again and again while HOLD is set -
\ and answers its argument plus the row's first cell.
: START-IMPL ( n -- n ) {: arg:n :}
   data-base CB-OWNER + atomic@ THREAD-OWNER !
   PTHREAD-SELF THREAD-SELF !
   PARK
   begin
      THREAD-ROW 200 ROW-FILL
      THREAD-ROW BYTE-VIEW ROW-N CELL CMP-FN @ QSORT
      THREAD-ROW 200 ROW-SORTED? 0= if 1 BAD atomic-add drop then
      1 THREAD-SORTS atomic-add drop
      HOLD atomic@ 0=
   until
   arg THREAD-ROW @ + ;

\ Bound and never reached: a second thread is refused before its dispatch.
: START-B-IMPL ( n -- n ) ;

: MARSHAL-FLOATS ( r r r r r r r r -- r )
   {: f0:r f1:r f2:r f3:r f4:r f5:r f6:r f7:r :}
   f0 0 SEEN-FLOATS !  f1 1 SEEN-FLOATS !  f2 2 SEEN-FLOATS !  f3 3 SEEN-FLOATS !
   f4 4 SEEN-FLOATS !  f5 5 SEEN-FLOATS !  f6 6 SEEN-FLOATS !  f7 7 SEEN-FLOATS !
   f7 ;

: MARSHAL-X64-IMPL ( n n n ptr u8 n n r r r r r r r r -- r )
   {: i0:n i1:n i2:n p3 i4:n i5:n f0:r f1:r f2:r f3:r f4:r f5:r f6:r f7:r :}
   i0 0 SEEN-INTS !  i1 1 SEEN-INTS !  i2 2 SEEN-INTS !  p3 c@ 3 SEEN-INTS !
   i4 4 SEEN-INTS !  i5 5 SEEN-INTS !
   f0 f1 f2 f3 f4 f5 f6 f7 MARSHAL-FLOATS ;

: MARSHAL-ARM-IMPL ( n n n ptr u8 n n n n r r r r r r r r -- r )
   {: i0:n i1:n i2:n p3 i4:n i5:n i6:n i7:n f0:r f1:r f2:r f3:r f4:r f5:r f6:r f7:r :}
   i0 0 SEEN-INTS !  i1 1 SEEN-INTS !  i2 2 SEEN-INTS !  p3 c@ 3 SEEN-INTS !
   i4 4 SEEN-INTS !  i5 5 SEEN-INTS !  i6 6 SEEN-INTS !  i7 7 SEEN-INTS !
   f0 f1 f2 f3 f4 f5 f6 f7 MARSHAL-FLOATS ;

\ The owner of the region this callback runs on, which is the calling thread.
: WHO-IMPL ( -- n )
   data-base CB-OWNER + atomic@ ;

' ROW-ORDER CMP-BODY !
' CMP-SELF-IMPL CMP-SELF-BODY !
' CMP-VIA-IMPL CMP-VIA-BODY !
' CMP-BOOM-IMPL CMP-BOOM-BODY !
' CMP-UNBIND-IMPL CMP-UNBIND-BODY !
' START-IMPL START-BODY !
' START-B-IMPL START-B-BODY !
: INSTALL-MARSHAL ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" ' MARSHAL-X64-IMPL MARSHAL-BODY !" evaluate-closed
   else
      s" ' MARSHAL-ARM-IMPL MARSHAL-BODY !" evaluate-closed
   then ;
INSTALL-MARSHAL
' WHO-IMPL WHO-BODY !
\ UNSET-BODY is left unset: its dispatch has to refuse, not execute the zero.

\ C calling an entry, the way lib/ffi-test.f reaches its own stubs: the raw
\ bounded calls, whose target is the entry address. A FUNCTION: declaration
\ resolves a symbol and an entry has none.
TRUSTED: CALL1 ( n n -- n ) {: arg:n fn:n :}
   FFI:RESET
   arg 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 fn ffi-call-bounded ;

\ Every argument register loaded: six/eight integers and eight floats.
TRUSTED: MARSHAL-CALL ( n -- r ) {: fn:n :}
   FFI:RESET
   $123456789ABCDEF0 0 FFI:VALUE!
   $1FFFFFFFE 1 FFI:VALUE!           \ a C int -2 under a dirty high half
   $7FFFFFFFD 2 FFI:VALUE!           \ an unsigned $FFFFFFFD under one
   MARSHAL-BYTE 3 FFI:READABLE!
   40 4 FFI:VALUE!  50 5 FFI:VALUE!
   HB-TARGET-LINUX-X86-64? 0= if
      60 6 FFI:VALUE!  70 7 FFI:VALUE!
   then
   1.0 0 FFI:FLOAT!  2.0 1 FFI:FLOAT!  3.0 2 FFI:FLOAT!  4.0 3 FFI:FLOAT!
   5.0 4 FFI:FLOAT!  6.0 5 FFI:FLOAT!  7.0 6 FFI:FLOAT!  8.0 7 FFI:FLOAT!
   FFI:ARGS FFI:FLOATS FFI:STACK FFI:REG-LENS FFI:STACK-LENS
   0 fn ffi-call-abi-r-bounded ;

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

\ ---- a foreign thread on an exposed task --------------------------------------
\ START and the comparator its body sorts through, both on CTX-TASK's context.
: THREAD-BIND ( -- )
   CTX-TASK TASK:EXPOSE
   CMP CTX-TASK TASK:CONTEXT ENTRY CMP-FN !
   START CTX-TASK TASK:CONTEXT ENTRY START-FN ! ;

\ pthread_create's own answer: 0, or its errno.
: THREAD-START ( n n -- n ) {: fn:n arg:n :}
   0 INSIDE atomic!
   0 GATE atomic!
   0 HOLD atomic!
   0 THREAD-SORTS atomic!
   THREAD BYTE-VIEW 0 fn arg PTHREAD-CREATE ;

: WAIT-INSIDE ( -- )
   begin INSIDE atomic@ 0= while TASK:PAUSE repeat ;

: THREAD-JOIN ( -- n )
   THREAD @ THREAD-RET BYTE-VIEW PTHREAD-JOIN ;

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

\ Exposes CTX-TASK, starts a thread on it and ends: the context outlives the
\ task that exposed it.
: WORK-EXPOSES ( -- )
   THREAD-BIND
   START-FN @ 7 THREAD-START TASK:RETURN ;

;using
;package

\ ffi-callback-child.f - one scenario per run, since a refusal ends the process;
\ the released case runs two that exit 0, and grown-bind exits 0 unless its
\ binding was lost.
\ Run: bin/hb --load test/ffi-callback-child.f -- <case>
require test/ffi-callback-fixture.f
require src/lib/string.f
require src/lib/task.f

package FFI-CB-TEST
using FFI-CB

70 constant SURVIVED-RC

\ A worker sorts through CMP-FN, which the scenario binds to the main region; a
\ non-zero argument first waits until the main thread is inside a foreign call
\ (CB-OWNER).
: RAID ( n -- ) {: wait:n :}
   PARK
   wait 0 <> if
      begin TASK:MAIN-BASE CB-OWNER + atomic@ 0= while TASK:PAUSE repeat
   then
   THREAD-ROW 200 ROW-FILL
   THREAD-ROW BYTE-VIEW ROW-N CELL CMP-FN @ QSORT ;

: STARTED ( n -- )
   0 <> if s" ffi-callback-child: pthread_create failed" SURVIVED-RC die then ;

: RAID-START ( [ -- ] -- ) {: body :}
   CMP TASK:SELF-CONTEXT ENTRY CMP-FN !
   body WORKER-A TASK:ACTIVATE
   WAIT-INSIDE ;

: OUTSIDE ( -- )
   [: 0 RAID ;] RAID-START
   s" outside: a thread is parked on its own context" type cr
   1 GATE atomic!
   begin GATE atomic@ drop again ;

: SLEEPING ( -- )
   [: 1 RAID ;] RAID-START
   s" sleeping: a thread is parked on its own context" type cr
   1 GATE atomic!
   10000 >MS TASK:SLEEP
   s" ffi-callback-child: sleeping was not refused" SURVIVED-RC die ;

: SORT-STALE ( -- )
   OUTER 10 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL CMP-FN @ QSORT ;

: UNBOUND ( -- )
   CMP TASK:SELF-CONTEXT ENTRY CMP-FN !
   SORT-PLAIN if s" unbound: sorted while bound" type cr then
   CMP UNBIND
   SORT-STALE
   s" ffi-callback-child: unbound was not refused" SURVIVED-RC die ;

: WORKER-STALE ( -- )
   ['] WORK-ENDS WORKER-B TASK:ACTIVATE
   WORKER-B TASK:JOIN JOINED SORTS-ALL = if
      s" worker-stale: the worker sorted and ended bound" type cr
   then
   SORT-STALE
   s" ffi-callback-child: worker-stale was not refused" SURVIVED-RC die ;

\ Defines once with no task live, so the definition is admitted. The thunk
\ restores C's callee-saved registers, the caller's record count and code
\ pointer among them, so it ends the process rather than lose what the
\ definition moved.
CALLBACK: CMP-DEFINE ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

variable DEFINED                        \ 1 once CMP-DEFINE has defined

: CMP-DEFINE-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   DEFINED @ 0= if
      1 DEFINED !
      s" variable CBC-INSIDE" INCLUDE-EVALUATE
   then
   a b ROW-ORDER ;

' CMP-DEFINE-IMPL CMP-DEFINE-BODY !

: BODY-DEFINES ( -- )
   CMP-DEFINE TASK:SELF-CONTEXT ENTRY {: fn:n :}
   OUTER 10 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL fn QSORT
   s" ffi-callback-child: body-defines was not refused" SURVIVED-RC die ;

\ PARK sees the halt and returns; TASK:PAUSE only yields while a comparison
\ runs, so qsort completes and the worker ends after C returns; KILL joins it.
CALLBACK: CMP-PARK ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

: CMP-PARK-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   PARK
   a b ROW-ORDER ;

' CMP-PARK-IMPL CMP-PARK-BODY !

: WORK-PARKS ( -- )
   CMP-PARK TASK:SELF-CONTEXT ENTRY {: fn:n :}
   OUTER 10 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL fn QSORT ;

: HALTED ( -- )
   ['] WORK-PARKS WORKER-A TASK:ACTIVATE
   WAIT-INSIDE
   s" halted: the worker is parked inside its comparator" type cr
   WORKER-A TASK:HALT
   WORKER-A TASK:KILL
   OUTER 10 ROW-SORTED? if
      s" halted: the comparator returned, qsort completed and KILL joined the worker"
      type cr
   then ;

\ The first comparison holds in TASK:WAIT until the kill's halt wakes it, so the
\ next TASK:PAUSE runs inside the callback with the request set; the sort
\ completes and the worker ends at its first TASK:PAUSE after it.
CALLBACK: CMP-YIELD ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

variable SORTED                      \ 1 once the killed worker's sort completed
variable PAST-PAUSE                  \ 1 if the worker ran past that TASK:PAUSE

: CMP-YIELD-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   INSIDE atomic@ 0= if 1 INSIDE atomic! TASK:WAIT then
   TASK:PAUSE
   a b ROW-ORDER ;

' CMP-YIELD-IMPL CMP-YIELD-BODY !

: WORK-YIELDS ( -- )
   CMP-YIELD TASK:SELF-CONTEXT ENTRY {: fn:n :}
   OUTER 40 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL fn QSORT
   OUTER 40 ROW-SORTED? if 1 SORTED atomic! then
   TASK:PAUSE
   1 PAST-PAUSE atomic! ;

: KILLED ( -- )
   0 INSIDE atomic!
   ['] WORK-YIELDS WORKER-B TASK:ACTIVATE
   WAIT-INSIDE
   s" killed: the worker is stopped inside its comparator" type cr
   WORKER-B TASK:KILL
   SORTED atomic@ 0 <> if s" killed: its sort completed" type cr then
   PAST-PAUSE atomic@ 0= if
      s" killed: the worker ended at its first TASK:PAUSE after the sort" type cr
   then ;

\ A thread C started is no task's, so a slot on the caller's context finds no
\ region for it.
: CALLER-THREAD ( -- )
   START TASK:CALLER-CONTEXT ENTRY {: fn:n :}
   s" caller-thread: the slot is bound to the caller's context" type cr
   fn 1 THREAD-START STARTED
   10000 >MS TASK:SLEEP
   s" ffi-callback-child: caller-thread was not refused" SURVIVED-RC die ;

\ Left unbound until a text that grows the tables binds it and throws.
CALLBACK: CMP-GROWN ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
' ROW-ORDER CMP-GROWN-BODY !
variable GROWN-FN                     \ the C entry the thrown text answered

: GROWN$ ( -- ptr u8 n )
   s" CALLBACK: CBC-GROW ( -- ) ;CALLBACK  CMP-GROWN TASK:SELF-CONTEXT FFI-CB:ENTRY GROWN-FN !  1 throw" ;

: GROWN-BIND ( -- )
   PAD-TO-EDGE
   [: GROWN$ evaluate-closed ;] catch 1 <> if
      s" ffi-callback-child: grown-bind's text did not throw 1" SURVIVED-RC die
   then
   OUTER 10 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL GROWN-FN @ QSORT
   OUTER 10 ROW-SORTED? if s" grown-bind: sorted" type cr then ;

: CASE? ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: DISPATCH ( -- )
   SCRIPT-ARGC 1 <> if s" ffi-callback-child: one case name" SURVIVED-RC die then
   s" outside" CASE? if OUTSIDE then
   s" sleeping" CASE? if SLEEPING then
   s" unbound" CASE? if UNBOUND then
   s" worker-stale" CASE? if WORKER-STALE then
   s" body-defines" CASE? if BODY-DEFINES then
   s" caller-thread" CASE? if CALLER-THREAD then
   s" released" CASE? if HALTED KILLED exit then
   s" grown-bind" CASE? if GROWN-BIND exit then
   s" ffi-callback-child: unknown case" SURVIVED-RC die ;

DISPATCH

;using
;package

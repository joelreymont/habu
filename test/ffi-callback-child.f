\ ffi-callback-child.f - one scenario per run, in its own process because a
\ wrong answer ends or wedges it. src/lib/ffi-callback-test.f spawns it per case
\ and reads the exit status and both streams. An unrefused scenario dies 70 by
\ name; the halted, killed, woken and fresh cases exit 0 once the task has left.
\ Run: bin/hb --load test/ffi-callback-child.f -- <case>
require test/ffi-callback-fixture.f
require src/lib/string.f
require src/lib/image-lifecycle.f
require src/lib/task.f

package FFI-CB-TEST
using FFI-CB

70 constant SURVIVED-RC

\ Sorts through CMP-FN, which the scenario binds to the main region; a non-zero
\ argument first waits until the main thread is inside a foreign call
\ (CB-OWNER).
CALLBACK: RAID ( n -- n ) 0 FALLBACK ;CALLBACK

: RAID-IMPL ( n -- n ) {: wait:n :}
   PARK
   wait 0 <> if
      begin TASK:MAIN-BASE CB-OWNER + atomic@ 0= while TASK:PAUSE repeat
   then
   THREAD-ROW 200 ROW-FILL
   THREAD-ROW BYTE-VIEW ROW-N CELL CMP-FN @ QSORT
   wait ;

' RAID-IMPL RAID-BODY !

: STARTED ( n -- )
   0 <> if s" ffi-callback-child: pthread_create failed" SURVIVED-RC die then ;

: RAID-START ( n -- ) {: wait:n :}
   CTX-TASK TASK:EXPOSE
   RAID CTX-TASK TASK:CONTEXT ENTRY {: fn:n :}
   CMP TASK:SELF-CONTEXT ENTRY CMP-FN !
   fn wait THREAD-START STARTED
   WAIT-INSIDE ;

: OUTSIDE ( -- )
   0 RAID-START
   s" outside: a thread is parked on its own context" type cr
   1 GATE atomic!
   begin GATE atomic@ drop again ;

: SLEEPING ( -- )
   1 RAID-START
   s" sleeping: a thread is parked on its own context" type cr
   1 GATE atomic!
   10000 >MS TASK:SLEEP
   s" ffi-callback-child: sleeping was not refused" SURVIVED-RC die ;

: BUSY ( -- )
   THREAD-BIND
   START-B CTX-TASK TASK:CONTEXT ENTRY {: second:n :}
   START-FN @ 1 THREAD-START STARTED
   WAIT-INSIDE
   s" busy: the first thread is parked inside the context" type cr
   second 2 THREAD-START STARTED
   10000 >MS TASK:SLEEP
   s" ffi-callback-child: busy was not refused" SURVIVED-RC die ;

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

: UNEXPOSED ( -- )
   THREAD-BIND
   CTX-TASK TASK:UNEXPOSE
   s" unexposed: the context is gone" type cr
   SORT-STALE
   s" ffi-callback-child: unexposed was not refused" SURVIVED-RC die ;

\ The ended worker's exposed CTX-TASK still counts on the main region, so the
\ main task may not lower the code pointer.
: DEFINES ( -- )
   ['] WORK-EXPOSES WORKER-A TASK:ACTIVATE
   WORKER-A TASK:JOIN JOINED 0= if s" defines: the exposing worker was joined" type cr then
   WAIT-INSIDE
   s" cp@ cp!" INCLUDE-EVALUATE
   s" ffi-callback-child: defines was not refused" SURVIVED-RC die ;

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

\ The first comparison holds in TASK:STOP until the kill's halt wakes it, so the
\ next TASK:PAUSE runs inside the callback with the request set; the sort
\ completes and the worker ends at its first TASK:PAUSE after it.
CALLBACK: CMP-YIELD ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

variable SORTED                      \ 1 once the killed worker's sort completed
variable PAST-PAUSE                  \ 1 if the worker ran past that TASK:PAUSE

: CMP-YIELD-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   INSIDE atomic@ 0= if 1 INSIDE atomic! TASK:STOP then
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
   ['] WORK-YIELDS WORKER-B TASK:ACTIVATE
   WAIT-INSIDE
   s" killed: the worker is stopped inside its comparator" type cr
   WORKER-B TASK:KILL
   SORTED atomic@ 0 <> if s" killed: its sort completed" type cr then
   PAST-PAUSE atomic@ 0= if
      s" killed: the worker ended at its first TASK:PAUSE after the sort" type cr
   then ;

\ On an exposed context TASK:SELF is the exposed task, so TASK:STOP parks on its
\ record and TASK:WAKE of the exposed task releases it.
CALLBACK: STOPS ( n -- n ) 0 FALLBACK ;CALLBACK

align
variable PAST-STOP                    \ 1 once STOPS's body has left its STOP

: STOPS-IMPL ( n -- n ) {: arg:n :}
   1 INSIDE atomic!
   TASK:STOP
   1 PAST-STOP atomic!
   arg 1 + ;

' STOPS-IMPL STOPS-BODY !

: WOKEN ( -- )
   CTX-TASK TASK:EXPOSE
   STOPS CTX-TASK TASK:CONTEXT ENTRY 3 THREAD-START STARTED
   WAIT-INSIDE
   s" woken: a thread is stopped inside its callback" type cr
   CTX-TASK TASK:WAKE
   THREAD-JOIN 0= THREAD-RET @ 4 = and if
      s" woken: WAKE of the exposed task released it to its result" type cr
   then
   CTX-TASK TASK:KILL ;

\ An exposure opens its park at zero, whatever earlier exposures of the task
\ posted; a stale hint would let the second exposure leave its STOP unwoken.
50 constant FRESH-MS                  \ the thread's time to take a stale hint

: PAUSES ( -- )
   begin TASK:PAUSE again ;

\ True when the thread stopped in this exposure waits for this exposure's WAKE
\ and that WAKE releases it to its result.
: STOPS-FRESH? ( -- bool )
   0 PAST-STOP atomic!
   STOPS CTX-TASK TASK:CONTEXT ENTRY 3 THREAD-START STARTED
   WAIT-INSIDE
   FRESH-MS >MS TASK:SLEEP
   PAST-STOP atomic@ 0=
   CTX-TASK TASK:WAKE
   THREAD-JOIN 0= and THREAD-RET @ 4 = and ;

: FRESH ( -- )
   ['] PAUSES CTX-TASK TASK:ACTIVATE
   CTX-TASK TASK:WAKE
   CTX-TASK TASK:KILL
   CTX-TASK TASK:EXPOSE
   STOPS-FRESH? if s" fresh: a hint to a run that ended does not reach an exposure" type cr then
   CTX-TASK TASK:WAKE
   CTX-TASK TASK:UNEXPOSE
   CTX-TASK TASK:EXPOSE
   STOPS-FRESH? if s" fresh: nor does a hint to an earlier exposure" type cr then
   CTX-TASK TASK:KILL ;

: CAPTURES ( -- )
   THREAD-BIND
   START-FN @ 1 THREAD-START STARTED
   WAIT-INSIDE
   s" captures: a thread is parked inside" type cr
   IMAGE-LIFECYCLE:PREPARE
   s" ffi-callback-child: captures was not refused" SURVIVED-RC die ;

: CASE? ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: DISPATCH ( -- )
   SCRIPT-ARGC 1 <> if s" ffi-callback-child: one case name" SURVIVED-RC die then
   s" outside" CASE? if OUTSIDE then
   s" sleeping" CASE? if SLEEPING then
   s" busy" CASE? if BUSY then
   s" unbound" CASE? if UNBOUND then
   s" worker-stale" CASE? if WORKER-STALE then
   s" unexposed" CASE? if UNEXPOSED then
   s" defines" CASE? if DEFINES then
   s" body-defines" CASE? if BODY-DEFINES then
   s" halted" CASE? if HALTED exit then
   s" killed" CASE? if KILLED exit then
   s" woken" CASE? if WOKEN exit then
   s" fresh" CASE? if FRESH exit then
   s" captures" CASE? if CAPTURES then
   s" ffi-callback-child: unknown case" SURVIVED-RC die ;

DISPATCH

;using
;package

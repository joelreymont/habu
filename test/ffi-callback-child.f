\ ffi-callback-child.f - one scenario per run, in a process of its own because
\ getting it wrong ends or wedges that process. Most are refusals the engine
\ ends the process on; one that is not refused dies 70 by name. The halted,
\ killed, woken and fresh cases stop a task or thread inside its callback and
\ exit 0 once it has left. lib/ffi-callback-test.f spawns this file once per
\ case and reads the exit status and both streams.
\ Run: bin/hb --load test/ffi-callback-child.f -- <case>
require test/ffi-callback-fixture.f
require lib/image-lifecycle.f
require lib/task.f

package FFI-CB-TEST
using FFI-CB

70 constant SURVIVED-RC

\ A start routine on its own exposed context whose body calls into ANOTHER
\ context: it sorts through CMP-FN, which the scenario binds to the main
\ region. A non-zero argument first waits for the main thread to be inside a
\ foreign call, which is what its CB-OWNER says.
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

\ The main context with no foreign call in flight: the main thread only spins.
: OUTSIDE ( -- )
   0 RAID-START
   s" outside: a thread is parked on its own context" type cr
   1 GATE atomic!
   begin GATE atomic@ drop again ;

\ The main context while its thread sleeps inside nanosleep.
: SLEEPING ( -- )
   1 RAID-START
   s" sleeping: a thread is parked on its own context" type cr
   1 GATE atomic!
   10000 >MS TASK:SLEEP
   s" ffi-callback-child: sleeping was not refused" SURVIVED-RC die ;

\ A second thread on a context the first is parked inside.
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

\ A worker bound the slots to its own context and ended without unbinding.
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

\ A worker exposed CTX-TASK and ended; the task still counts on the main
\ region, so the main task may not define.
: DEFINES ( -- )
   ['] WORK-EXPOSES WORKER-A TASK:ACTIVATE
   WORKER-A TASK:JOIN JOINED 0= if s" defines: the exposing worker was joined" type cr then
   WAIT-INSIDE
   s" variable CBC-LATE" INCLUDE-EVALUATE
   s" ffi-callback-child: defines was not refused" SURVIVED-RC die ;

\ A comparator on the main task that defines once, with no task live, so the
\ definition itself is admitted. The thunk restores C's callee-saved registers
\ on its way out, the caller's record count and code pointer among them, so it
\ ends the process rather than lose where the definition moved them.
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

\ A worker halted while parked inside its own comparator. PARK sees the request
\ and returns, TASK:PAUSE only yields while a comparison runs, qsort completes,
\ and the worker ends after C has returned; KILL joins it.
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

\ A worker killed in the middle of a sort whose comparator only yields. The
\ first comparison holds in TASK:STOP until the kill's halt wakes it, so the
\ TASK:PAUSE after it runs inside the callback with the request set. The sort
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

\ A foreign thread stopped inside its callback on an exposed context: TASK:SELF
\ there is the exposed task, so TASK:STOP parks on that task's record, and
\ TASK:WAKE of the exposed task releases it.
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

\ An exposure opens its park at zero, whatever earlier runs and exposures of the
\ same task posted to it. On lib/task.f before EXPOSE drained the park, the
\ second exposure's thread took the hint posted to the first and left its STOP
\ with no WAKE of its own, and the second line was missing.
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

\ A capture while a thread is parked inside a callback.
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

\ crew-test.f - CREW's tasks: three that record their task by value and end, a
\ fourth START that takes a reaped task's record, STOP ending parked tasks
\ through their finally, START inside STOP's quotation refused while STOP still
\ ends its tasks, and a body that throws reaped by the next START.
\ Run: bin/hb --load src/lib/crew-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/task.f
require src/lib/crew.f

package CREW-TEST
private

1000000 constant NS-PER-MS
10000 constant WAIT-MS               \ a gate host starts threads late
8 constant JOBS                      \ the values the cases use, 0..JOBS-1

\ Cells the tasks write, indexed by value.
JOBS TYPED-BUFFER SEEN n             \ the task that ran the value, 0 before
JOBS TYPED-BUFFER PARKED n           \ 1 once the body parks
JOBS TYPED-BUFFER ASKED n            \ 1 once STOP's quotation ran with it
JOBS TYPED-BUFFER UNWOUND n          \ 1 once the parked body's finally ran

CAST: N>TCB ( n -- ptr n )

: RESET ( -- )
   JOBS 0 ?do
      0 i SEEN atomic! 0 i PARKED atomic! 0 i ASKED atomic! 0 i UNWOUND atomic!
   loop ;

: WAIT ( [ -- bool ] -- )
   mono-ns WAIT-MS NS-PER-MS * + s" crew-test" TEST-WAIT:POLL ;

: SEEN? ( n -- bool ) SEEN atomic@ 0<> ;
: SEEN@ ( n -- n ) SEEN atomic@ ;
: SET ( ptr n -- ) 1 swap atomic! ;
: ONE? ( ptr n -- bool ) atomic@ 1 = ;

\ The value's task ran and has ended.
: ENDED? ( n -- bool )
   SEEN@ dup 0= if drop false exit then
   N>TCB TASK:DONE? ;

: RECORD ( n -- )
   {: v:n :}
   TASK:SELF-N v SEEN atomic! ;

: START-RECORD ( n -- ) TASK:MIN-STACK swap [: RECORD ;] CREW:START ;


\ The parked body's own value: the one whose SEEN is this task among those
\ parked.
: UNWIND-MINE ( -- )
   JOBS 0 ?do
      i PARKED ONE? i SEEN@ TASK:SELF-N = and if i UNWOUND SET unloop exit then
   loop ;

: PARK ( n -- )
   {: v:n :}
   TASK:SELF-N v SEEN atomic!
   v PARKED SET
   [: begin TASK:STOP TASK:PAUSE again ;] [: UNWIND-MINE ;] finally ;

: START-PARK ( n -- ) TASK:MIN-STACK swap [: PARK ;] CREW:START ;

: ASK ( n -- ) ASKED SET ;


: FAIL ( n -- )
   {: v:n :}
   TASK:SELF-N v SEEN atomic!
   1 0 / drop ;


: RECORD-CASE ( -- )
   RESET
   0 START-RECORD 1 START-RECORD 2 START-RECORD
   [: 0 ENDED? 1 ENDED? and 2 ENDED? and ;] WAIT
   s" three tasks each run the body with their own value" T-LABEL
   0 SEEN@ 1 SEEN@ T<>
   1 SEEN@ 2 SEEN@ T<>
   0 SEEN@ 2 SEEN@ T<>
   3 START-RECORD
   [: 3 SEEN? ;] WAIT
   \ TASK:NEW takes the first free record in pool order: the first task's once
   \ this START's reap gave all three back, a fresh one without the reap.
   s" a later START takes the record of a task it reaped" T-LABEL
   3 SEEN@ 0 SEEN@ T= ;


: STOP-CASE ( -- )
   RESET
   4 START-PARK 5 START-PARK 6 START-PARK
   [: 4 PARKED ONE? 5 PARKED ONE? and 6 PARKED ONE? and ;] WAIT
   [: ASK ;] CREW:STOP
   s" STOP runs its quotation with every held task's value" T-LABEL
   4 ASKED ONE? TTRUE 5 ASKED ONE? TTRUE 6 ASKED ONE? TTRUE
   s" STOP ends parked tasks through their finally" T-LABEL
   4 UNWOUND ONE? TTRUE 5 UNWOUND ONE? TTRUE 6 UNWOUND ONE? TTRUE ;


: START-INSIDE ( n -- ) drop 0 START-RECORD ;

: STOP-START ( -- ) [: START-INSIDE ;] CREW:STOP ;

: STOP-INSIDE ( n -- ) drop [: drop ;] CREW:STOP ;

: STOP-STOP ( -- ) [: STOP-INSIDE ;] CREW:STOP ;

: INSIDE-CASE ( -- )
   RESET
   7 START-PARK
   [: 7 PARKED ONE? ;] WAIT
   s" START inside STOP's quotation is E-STATE" T-LABEL
   [: STOP-START ;] CREW:E-STATE TTHROWSQ
   s" STOP ends its tasks when the quotation throws" T-LABEL
   7 UNWOUND ONE? TTRUE
   RESET
   7 START-PARK
   [: 7 PARKED ONE? ;] WAIT
   s" STOP inside STOP's quotation is E-STATE" T-LABEL
   [: STOP-STOP ;] CREW:E-STATE TTHROWSQ
   7 UNWOUND ONE? TTRUE ;


\ The thrower's stderr line is not asserted.
: THROW-CASE ( -- )
   RESET
   TASK:MIN-STACK 0 [: FAIL ;] CREW:START
   [: 0 ENDED? ;] WAIT
   SB-RESET s" kept" SB-APPEND
   1 START-RECORD
   SB$ nip {: kept:n :}
   s" START leaves the caller's string builder alone" T-LABEL
   kept 4 T=
   [: 1 SEEN? ;] WAIT
   s" the next START reaps a task that threw" T-LABEL
   1 SEEN@ 0 SEEN@ T=
   2 START-RECORD
   [: 2 SEEN? ;] WAIT
   [: drop ;] CREW:STOP ;

\ A data stack no host can map: TASK:ACTIVATE throws inside START.
: HUGE ( -- ) 1 62 lshift 1 [: RECORD ;] CREW:START ;

: ACTIVATE-CASE ( -- )
   RESET
   0 START-RECORD
   [: 0 ENDED? ;] WAIT
   s" a START whose stacks cannot be mapped is E-MEM-MAP" T-LABEL
   [: HUGE ;] E-MEM-MAP TTHROWSQ
   2 START-RECORD
   [: 2 SEEN? ;] WAIT
   \ The refused START reaped the first task and took its record, so the next
   \ START takes that record only if the refused one gave it back.
   s" a START whose task cannot run gives its record back" T-LABEL
   2 SEEN@ 0 SEEN@ T=
   [: drop ;] CREW:STOP ;

public

: MAIN ( -- )
   T-RESET
   RECORD-CASE
   STOP-CASE
   INSIDE-CASE
   THROW-CASE
   ACTIVATE-CASE
   T-REPORT ;

MAIN

;package

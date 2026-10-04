\ headless-host.f - a host for RT-SCOPE's port with no browser (§24.5, §28.3).
\ It answers SUBMIT from a recorded script, accepted once the script runs dry,
\ and wakes the runtime again after backpressure as a host with room would;
\ queues WAKE for a later WAKE step instead of turning inside it; issues host
\ handles from its own table and frees a released handle, or a closed scope's,
\ when it delivers done for the release or close; tears down every handle it
\ holds when it answers stale-epoch, the incarnation's end; and writes each
\ step, port call and the scope states after each step to a trace of six-cell
\ records. EXPORT writes the trace to a file, a record as six little-endian
\ cells. REPLAY-FILE runs a trace file's steps from a fresh epoch, answering
\ each SUBMIT as recorded, and answers whether it wrote the same trace; it
\ runs in a fresh process, since a process holds one epoch at a time. A trace
\ names scopes and requests by refs, their order of first sight, and host
\ handles by their host table index + 1, so it holds no run's serials. Each
\ run takes fresh pools for its epoch's records.

require lib/errors.f
require lib/fs.f
require lib/test.f
require lib/runtime/id.f
require lib/runtime/handle.f
require lib/runtime/pool.f
require lib/runtime/scope.f

package HEADLESS

public

1 constant T-OPEN       \ parent ref, kind tag, throw code at e
2 constant T-CLOSE      \ scope ref
3 constant T-REQUEST    \ scope ref
5 constant T-STEP-WAKE  \ the host runs the turn a WAKE asked for
6 constant T-DELIVER    \ request ref, outcome: 0 done, 1 done holding a new handle, 2 failed
7 constant T-TIME       \ root time in whole milliseconds
8 constant T-SUBMIT     \ request ref, kind tag, scope ref, target ref, answer tag
9 constant T-WAKE
10 constant T-STATE     \ scope ref, state tag or RETIRED; then live host handles
7 constant RETIRED

private

6 constant REC-CELLS
1024 constant TRACE-CAP
TRACE-CAP REC-CELLS * TYPED-BUFFER TRACE n
TRACE-CAP REC-CELLS * TYPED-BUFFER SOURCE n
variable TRACE-LEN
variable SOURCE-LEN
variable SOURCE-AT

32 constant REF-CAP
REF-CAP TYPED-BUFFER SCOPE-REFS RT-SCOPE:scope
REF-CAP TYPED-BUFFER SCOPE-IDS n
variable SCOPE-COUNT
64 constant REQ-CAP
REQ-CAP TYPED-BUFFER REQ-REFS RT-SCOPE:rt-request
REQ-CAP TYPED-BUFFER REQ-IDS n
variable REQ-COUNT

64 constant SCRIPT-CAP
SCRIPT-CAP TYPED-BUFFER SCRIPT n
variable SCRIPT-LEN
variable SCRIPT-AT

\ Each run takes fresh cells for its epoch, pools and host table: a closed
\ counter's cells never make a counter again, pools' cells and a table's
\ header open once.
12 constant RUN-CAP
16 constant HSLOT-CAP
RUN-CAP HSLOT-CAP * TYPED-BUFFER HSLOTS n
RUN-CAP RT-HANDLE:HEADER-CELLS * TYPED-BUFFER HHEADERS n
RUN-CAP RT-SCOPE:EPOCH-CELLS * TYPED-BUFFER ECELLS n
RUN-CAP RT-POOL:POOLS-CELLS * TYPED-BUFFER PCELLS n
TYPED-VARIABLE HTABLE RT-HANDLE:table
TYPED-VARIABLE RUN-POOLS RT-POOL:pools
\ The handles the host holds, by host table index; null where none.
HSLOT-CAP TYPED-BUFFER HOLDING RT-HANDLE:handle
variable RUN
variable REPLAYING
variable PENDING
variable LIVE-HANDLES
variable SUBMITS
variable LAST

: EMIT ( n n n n n n -- )
   {: t:n a:n b:n c:n d:n e:n :}
   TRACE-LEN @ TRACE-CAP >= if s" headless-host: trace full" 70 die then
   TRACE-LEN @ REC-CELLS * {: at:n :}
   t at TRACE !  a at 1 + TRACE !  b at 2 + TRACE !
   c at 3 + TRACE !  d at 4 + TRACE !  e at 5 + TRACE !
   TRACE-LEN @ 1 + TRACE-LEN ! ;

: >ANSWER ( n -- RT-SCOPE:submit-result )
   case
      0 of RT--SCOPE-SUBMIT--RESULT:accepted endof
      1 of RT--SCOPE-SUBMIT--RESULT:backpressure endof
      2 of RT--SCOPE-SUBMIT--RESULT:invalid endof
      3 of RT--SCOPE-SUBMIT--RESULT:denied endof
      4 of RT--SCOPE-SUBMIT--RESULT:unavailable endof
      5 of RT--SCOPE-SUBMIT--RESULT:oom endof
      6 of RT--SCOPE-SUBMIT--RESULT:stale-epoch endof
      RT--SCOPE-SUBMIT--RESULT:host-failed swap
   endcase ;

: >KIND ( n -- RT-SCOPE:scope-kind )
   case
      0 of RT--SCOPE-SCOPE--KIND:application endof
      1 of RT--SCOPE-SCOPE--KIND:document endof
      2 of RT--SCOPE-SCOPE--KIND:component endof
      3 of RT--SCOPE-SCOPE--KIND:gesture endof
      4 of RT--SCOPE-SCOPE--KIND:job endof
      RT--SCOPE-SCOPE--KIND:plugin swap
   endcase ;

\ The next answer: the script's in a recorded run, the trace file's next
\ SUBMIT record's in a replay (host-failed past its last).
: SOURCE-TAG ( n -- n )
   REC-CELLS * SOURCE @ ;

: NEXT-ANSWER ( -- n )
   REPLAYING @ 0= if
      SCRIPT-AT @ SCRIPT-LEN @ >= if 0 exit then
      SCRIPT-AT @ SCRIPT @  SCRIPT-AT @ 1 + SCRIPT-AT !  exit
   then
   begin SOURCE-AT @ SOURCE-LEN @ < while
      SOURCE-AT @ 1 + SOURCE-AT !
      SOURCE-AT @ 1 - SOURCE-TAG T-SUBMIT = if SOURCE-AT @ 1 - REC-CELLS * 5 + SOURCE @ exit then
   repeat
   7 ;

\ The ref of a request, assigned when the host first sees it.
: REQ-REF ( RT-SCOPE:rt-request -- n )
   {: r:RT-SCOPE:rt-request :}
   r RT-SCOPE:REQUEST-ID {: id:n :}
   REQ-COUNT @ 0 ?do i REQ-IDS @ id = if i 1 + unloop exit then loop
   REQ-COUNT @ REQ-CAP >= if s" headless-host: too many requests" 70 die then
   r REQ-COUNT @ REQ-REFS !
   id REQ-COUNT @ REQ-IDS !
   REQ-COUNT @ 1 + REQ-COUNT !
   REQ-COUNT @ ;

: SCOPE-REF ( RT-SCOPE:scope -- n )
   RT-SCOPE:LOCAL-ID {: id:n :}
   SCOPE-COUNT @ 0 ?do i SCOPE-IDS @ id = if i 1 + unloop exit then loop
   0 ;

: >SCOPE ( n -- RT-SCOPE:scope )
   {: ref:n :}
   ref 0= if RT-SCOPE:NO-SCOPE exit then
   ref 1 - SCOPE-REFS @ ;

: >REQUEST ( n -- RT-SCOPE:rt-request )
   1 - REQ-REFS @ ;

: HANDLE-REF ( RT-HANDLE:handle -- n )
   {: h:RT-HANDLE:handle :}
   h RT-HANDLE:NULL? if 0 exit then
   h HTABLE @ RT-HANDLE:INDEX 1 + ;

\ ---- host handles -----------------------------------------------------------

: ISSUE ( -- RT-HANDLE:handle )
   HTABLE @ RT-HANDLE:ISSUE {: h:RT-HANDLE:handle :}
   h h HTABLE @ RT-HANDLE:INDEX HOLDING !
   LIVE-HANDLES @ 1 + LIVE-HANDLES !
   h ;

: FREE ( RT-HANDLE:handle -- )
   {: h:RT-HANDLE:handle :}
   h HTABLE @ RT-HANDLE:INDEX {: i:n :}
   h HTABLE @ RT-HANDLE:RELEASE
   RT-HANDLE:NULL i HOLDING !
   LIVE-HANDLES @ 1 - LIVE-HANDLES ! ;

\ The incarnation's end: the host frees every handle it holds.
: TEARDOWN ( -- )
   HSLOT-CAP 0 do i HOLDING @ RT-HANDLE:NULL? 0= if i HOLDING @ FREE then loop ;

: HOST-SUBMIT ( RT-SCOPE:rt-request -- RT-SCOPE:submit-result )
   {: r:RT-SCOPE:rt-request :}
   NEXT-ANSWER {: a:n :}
   r RT-SCOPE:KIND {: k:RT-SCOPE:request-kind :}
   T-SUBMIT r REQ-REF k RT--SCOPE-REQUEST--KIND:TAG
   r RT-SCOPE:OWNER SCOPE-REF r RT-SCOPE:TARGET HANDLE-REF a EMIT
   SUBMITS @ 1 + SUBMITS !
   a 1 = if 1 PENDING ! then
   a 6 = if TEARDOWN then
   a >ANSWER ;

: HOST-WAKE ( -- )
   T-WAKE 0 0 0 0 0 EMIT
   1 PENDING ! ;

: BIND ( -- )
   [: HOST-SUBMIT ;] is RT-SCOPE:SUBMIT
   [: HOST-WAKE ;] is RT-SCOPE:WAKE ;

BIND

\ The state tag of the scope at the ref, kept on the stack across the catch.
: TRY-TAG ( n n -- n n )
   swap drop dup >SCOPE RT-SCOPE:STATE@ RT--SCOPE-SCOPE--STATE:TAG swap ;

: STATE-TAG ( n -- n )
   RETIRED swap [: TRY-TAG ;] catch {: code:n :}
   code 0= if drop exit then
   2drop
   code RT-SCOPE:E-RT-SCOPE-STALE <> if code throw then
   RETIRED ;

\ After each step: every scope's state, then the live host handles.
: SNAP ( -- )
   SCOPE-COUNT @ 0 ?do T-STATE i 1 + dup STATE-TAG 0 0 0 EMIT loop
   T-STATE 0 LIVE-HANDLES @ 0 0 0 EMIT ;

: NOTE ( n n n n -- )
   {: t:n a:n b:n code:n :}
   code LAST !
   t a b 0 0 code EMIT
   SNAP ;

: ADD-SCOPE ( RT-SCOPE:scope -- )
   {: s:RT-SCOPE:scope :}
   SCOPE-COUNT @ REF-CAP >= if s" headless-host: too many scopes" 70 die then
   s SCOPE-COUNT @ SCOPE-REFS !
   s RT-SCOPE:LOCAL-ID SCOPE-COUNT @ SCOPE-IDS !
   SCOPE-COUNT @ 1 + SCOPE-COUNT ! ;

: DO-OPEN ( n n -- n n )
   {: p:n k:n :}
   p >SCOPE k >KIND RT-SCOPE:OPEN ADD-SCOPE
   p k ;

: DO-CLOSE ( n -- n )
   dup >SCOPE RT-SCOPE:CLOSE ;

: DO-REQUEST ( n -- n )
   dup >SCOPE RT-SCOPE:REQUEST REQ-REF drop ;

: DO-TIME ( n -- n )
   dup s>f RT-SCOPE:TIME! ;

: >OUTCOME ( n -- RT-SCOPE:outcome-record )
   {: how:n :}
   how 2 = if RT--ID-ERROR--CLASS:platform 1 RT--SCOPE-OUTCOME--RECORD:failed exit then
   how 0= if RT-HANDLE:NULL RT--SCOPE-OUTCOME--RECORD:done exit then
   ISSUE RT--SCOPE-OUTCOME--RECORD:done ;

\ A done release or close is one the host completed first: it frees the
\ released handle, or the closed scope's.
: DO-DELIVER ( n n -- n n )
   {: ref:n how:n :}
   ref >REQUEST {: r:RT-SCOPE:rt-request :}
   r RT-SCOPE:KIND {: k:RT-SCOPE:request-kind :}
   k RT--SCOPE-REQUEST--KIND:release RT--SCOPE-REQUEST--KIND:EQ
   k RT--SCOPE-REQUEST--KIND:scope-close RT--SCOPE-REQUEST--KIND:EQ or
   how 0= and if r RT-SCOPE:TARGET FREE then
   r how >OUTCOME RT-SCOPE:DELIVER
   ref how ;

: RESET ( -- )
   RUN @ RUN-CAP >= if s" headless-host: out of fresh runs" 70 die then
   RUN @ 0<> if RUN-POOLS @ RT-POOL:SHUTDOWN then
   0 TRACE-LEN !  0 SCOPE-COUNT !  0 REQ-COUNT !  0 SCRIPT-LEN !  0 SCRIPT-AT !
   0 PENDING !  0 LIVE-HANDLES !  0 SUBMITS !  0 LAST !  0 SOURCE-AT !
   HSLOT-CAP 0 do RT-HANDLE:NULL i HOLDING ! loop
   RUN @ HSLOT-CAP * HSLOTS HSLOT-CAP 1 RUN @ RT-HANDLE:HEADER-CELLS * HHEADERS RT-HANDLE:OPEN HTABLE !
   RUN @ RT-POOL:POOLS-CELLS * PCELLS RT-POOL:INIT RUN-POOLS !
   RUN @ RT-SCOPE:EPOCH-CELLS * ECELLS RUN-POOLS @ RT-SCOPE:START
   RUN @ 1 + RUN ! ;

public

\ Steps. Each runs one runtime call under catch, records itself with the
\ code it threw (0 for none) and then the states.
: OPEN-STEP ( n n -- )
   {: p:n k:n :}
   p k [: DO-OPEN ;] catch {: code:n :} 2drop
   T-OPEN p k code NOTE ;

: CLOSE-STEP ( n -- )
   {: ref:n :}
   ref [: DO-CLOSE ;] catch {: code:n :} drop
   T-CLOSE ref 0 code NOTE ;

: REQUEST-STEP ( n -- )
   {: ref:n :}
   ref [: DO-REQUEST ;] catch {: code:n :} drop
   T-REQUEST ref 0 code NOTE ;

: TIME-STEP ( n -- )
   {: ms:n :}
   ms [: DO-TIME ;] catch {: code:n :} drop
   T-TIME ms 0 code NOTE ;

: DELIVER-STEP ( n n -- )
   {: ref:n how:n :}
   ref how [: DO-DELIVER ;] catch {: code:n :} 2drop
   T-DELIVER ref how code NOTE ;

\ Run the turn a WAKE asked for, if one did; a = 1 when a turn ran.
: WAKE-STEP ( -- )
   PENDING @ 0= if T-STEP-WAKE 0 0 0 NOTE exit then
   0 PENDING !
   [: RT-SCOPE:TURN ;] catch {: code:n :}
   T-STEP-WAKE 1 0 code NOTE ;

\ The answer the next SUBMIT gets, as a submit-result tag.
: ANSWER ( n -- )
   SCRIPT-LEN @ SCRIPT-CAP >= if s" headless-host: script full" 70 die then
   SCRIPT-LEN @ SCRIPT !
   SCRIPT-LEN @ 1 + SCRIPT-LEN ! ;

: BEGIN-RUN ( -- )
   0 REPLAYING ! RESET ;

: LAST-CODE ( -- n ) LAST @ ;
: SUBMIT-COUNT ( -- n ) SUBMITS @ ;
: HOST-LIVE ( -- n ) LIVE-HANDLES @ ;
: PENDING? ( -- bool ) PENDING @ 0<> ;
: STATE-OF ( n -- n ) STATE-TAG ;
: REQUEST-OF ( n -- RT-SCOPE:rt-request ) >REQUEST ;
: SCOPE-OF ( n -- RT-SCOPE:scope ) >SCOPE ;
: POOLS-OF ( -- RT-POOL:pools ) RUN-POOLS @ ;
: TRACE-RECORDS ( -- n ) TRACE-LEN @ ;

\ The kind tag and target ref of the nth SUBMIT of this run, from 1.
: SUBMITTED ( n -- n n )
   {: want:n :}
   0 TRACE-LEN @ 0 ?do
      i REC-CELLS * TRACE @ T-SUBMIT = if 1 + dup want = if
         drop i REC-CELLS * 2 + TRACE @ i REC-CELLS * 4 + TRACE @ unloop exit
      then then
   loop
   drop -1 -1 ;

\ Write this run's trace to the file at the path.
: EXPORT ( ptr u8 n -- )
   0 TRACE BYTE-VIEW TRACE-LEN @ REC-CELLS * cells WRITE-ALL ;

private

: PLAY ( n -- )
   REC-CELLS * {: at:n :}
   at SOURCE @ {: t:n :}
   at 1 + SOURCE @ {: a:n :}
   at 2 + SOURCE @ {: b:n :}
   t T-OPEN = if a b OPEN-STEP exit then
   t T-CLOSE = if a CLOSE-STEP exit then
   t T-REQUEST = if a REQUEST-STEP exit then
   t T-STEP-WAKE = if WAKE-STEP exit then
   t T-DELIVER = if a b DELIVER-STEP exit then
   t T-TIME = if a TIME-STEP then ;

\ Is this run's trace the trace file's, cell for cell?
: SAME? ( -- bool )
   TRACE-LEN @ SOURCE-LEN @ <> if false exit then
   TRACE-LEN @ REC-CELLS * 0 ?do i TRACE @ i SOURCE @ <> if false unloop exit then loop
   true ;

public

\ Run the steps of the trace file at the path from a fresh epoch, answering
\ each SUBMIT as the file records, and answer whether this run wrote the same
\ trace.
: REPLAY-FILE ( ptr u8 n -- bool )
   0 SOURCE BYTE-VIEW TRACE-CAP REC-CELLS * cells READ-ALL
   REC-CELLS cells / SOURCE-LEN !
   1 REPLAYING ! RESET
   SOURCE-LEN @ 0 ?do i PLAY loop
   0 REPLAYING !
   SAME? ;

;package

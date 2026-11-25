\ memory-test.f - focused tests for OS-backed memory buffers.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/test/ending.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/test/subject.f
require src/lib/task.f
require src/lib/test/wait.f
require src/lib/adt/result.f                 \ a halted task's join answer is MATCHed

64 constant MEMT-BUFS
16 constant MEMT-SPAN-BUFS
32 constant MEMT-SPANS
65 constant MEMT-MARK-A
90 constant MEMT-MARK-Z

variable MEMT-HERE

: MEMT-TOTAL ( -- n )
   MEMT-BUFS MEM-64K * ;

: MEMT-SPAN-TOTAL ( -- n )
   MEMT-SPAN-BUFS MEM-64K * ;

: MEMT-END ( str -- ^u8 ) {: s:str :}
   s STR:DATA s STR:LEN 1 - + ;

: MEMT-TOUCH-ENDS ( str -- ) {: s:str :}
   MEMT-MARK-A s STR:DATA c!
   MEMT-MARK-Z s MEMT-END c!
   s STR:DATA c@ MEMT-MARK-A T=
   s MEMT-END c@ MEMT-MARK-Z T= ;

: MEMT-TOUCH-SLOTS ( str n -- ) {: s:str count :}
   count MEM-64K * s STR:LEN T=
   count 0 ?do
      MEMT-MARK-A i + s STR:DATA i MEM-64K * + c!
      s STR:DATA i MEM-64K * + c@ MEMT-MARK-A i + T=
   loop ;

: MEMT-TOUCH-64K-SLOTS ( str -- )
   MEMT-BUFS MEMT-TOUCH-SLOTS ;

: MEMT-TOUCH-SPAN-SLOTS ( str -- )
   MEMT-SPAN-BUFS MEMT-TOUCH-SLOTS ;

: MEMT-CELL-SPAN ( -- )
   4 >COUNT MEM-ALLOC-CELLS {: a:^ :}
   111 a !
   222 a 1 cells + !
   a @ 111 T=
   a 1 cells + @ 222 T= ;

: MEMT-SINGLE-64K ( -- )
   MEM-ALLOC-64K
   dup STR:LEN MEM-64K T=
   MEMT-TOUCH-ENDS ;

: MEMT-MANY-64K ( -- )
   MEMT-BUFS MEM-ALLOC-64K-BUFFERS
   MEMT-TOUCH-64K-SLOTS ;

: MEMT-DATA-UNCHANGED ( -- )
   here NULL-PTR - MEMT-HERE !
   MEM-ALLOC-64K drop
   here NULL-PTR - MEMT-HERE @ T= ;

: MEMT-LIVE-SPAN-FRAME ( str n -- ) {: s:str remaining :}
   s STR:LEN MEMT-SPAN-TOTAL T=
   s MEMT-TOUCH-SPAN-SLOTS
   remaining 0 > if
      MEMT-SPAN-BUFS MEM-ALLOC-64K-BUFFERS
      remaining 1 - recurse
   then
   s MEMT-TOUCH-SPAN-SLOTS ;

: MEMT-MANY-LIVE-SPANS ( -- )
   here NULL-PTR - MEMT-HERE !
   MEMT-SPAN-BUFS MEM-ALLOC-64K-BUFFERS
   MEMT-SPANS 1 - MEMT-LIVE-SPAN-FRAME
   here NULL-PTR - MEMT-HERE @ T= ;


T-RESET
MEM-64K $10000 T=
MEM-MAP-SHARED 1 T=
1 MEM-64K-BYTES MEM-64K T=
1 MEM-64K-COUNT-FOR 1 T=
MEM-64K MEM-64K-COUNT-FOR 1 T=
MEM-64K 1 + MEM-64K-COUNT-FOR 2 T=
MEM-CELL-BYTES 1 cells T=
1 >COUNT MEM-CELLS>BYTES MEM-CELL-BYTES T=
1 MEM-64K-SPAN-BYTES MEM-64K T=
MEM-64K 1 + MEM-64K-SPAN-BYTES MEM-64K 2 * T=
MEMT-CELL-SPAN
MEMT-SINGLE-64K
MEMT-MANY-64K
MEMT-DATA-UNCHANGED
MEMT-MANY-LIVE-SPANS
T-REPORT

false [if] \ off until dot habu-add-the-checks-a49d054d
: MEMT-ZERO-BYTES ( -- )
   0 MEM-ALLOC-BYTES 2drop ;

: MEMT-NEG-BYTES ( -- )
   -1 MEM-ALLOC-BYTES 2drop ;

: MEMT-ZERO-64K ( -- )
   0 MEM-ALLOC-64K-BUFFERS drop ;

: MEMT-TOO-MANY-64K ( -- )
   MEM-MAX-64K-BUFFERS 1 + MEM-64K-BYTES drop ;

: MEMT-TOO-LARGE-SPAN ( -- )
   MEM-MAX-N MEM-ALLOC-64K-SPAN drop ;

: MEMT-ZERO-CELLS ( -- )
   0 >COUNT MEM-ALLOC-CELLS drop ;

: MEMT-TOO-MANY-CELLS ( -- )
   MEM-MAX-CELLS 1 + >COUNT MEM-CELLS>BYTES drop ;

T-RESET
' MEMT-ZERO-BYTES E-MEM-SIZE TTHROWS
' MEMT-NEG-BYTES E-MEM-SIZE TTHROWS
' MEMT-ZERO-64K E-MEM-SIZE TTHROWS
' MEMT-TOO-MANY-64K E-MEM-SIZE TTHROWS
' MEMT-TOO-LARGE-SPAN E-MEM-SIZE TTHROWS
' MEMT-ZERO-CELLS E-MEM-SIZE TTHROWS
' MEMT-TOO-MANY-CELLS E-MEM-SIZE TTHROWS
T-REPORT
[then]

\ The returned alloc role's raw projection is MEM-private, so the tests touch
\ memory over the KNOWN raw extent and drop the role.
\ MEM:WITH-BYTES releases the mapping on scope exit. The body is a named word
\ because a quotation cannot hold locals.
: MEMT-TYPED-BYTES-BODY ( ^u8 NUM:alloc-byte-len -- ) {: a:^ len :}
   MEMT-MARK-A a c!  MEMT-MARK-Z a MEM-64K 1 - + c!
   a c@ MEMT-MARK-A T=  a MEM-64K 1 - + c@ MEMT-MARK-Z T= ;
: MEMT-TYPED-BYTES ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN [: MEMT-TYPED-BYTES-BODY ;] MEM:WITH-BYTES ;
: MEMT-TYPED-64K ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: a:^ :}
   MEMT-MARK-A a c!  MEMT-MARK-Z a MEM-64K 1 - + c!
   a c@ MEMT-MARK-A T=  a MEM-64K 1 - + c@ MEMT-MARK-Z T= ;
: MEMT-TYPED-CELLS ( -- )
   4 MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: a:^ :}
   111 a !  222 a 1 cells + !
   a @ 111 T=  a 1 cells + @ 222 T= ;

using MEM
package MEMT
private

$1000 constant RBT-CAP
-7777 constant E-PRIMARY

create RBT-OUT RBT-CAP allot
create RBT-ERR RBT-CAP allot

: MEMT-EXIT0 ( -- )
   s" " EXIT-STATUS:SUCCESS die ;

: MEMT-FORK ( [ -- ] -- PROCESS:status )
   {: body :}
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if
      body execute
      MEMT-EXIT0
   then
   pid PROCESS:WAIT ;

\ The child ran to a clean exit. A blocking wait has no deadline, so there is no
\ capture to report and the outcome reads as the child's rc.
: MEMT-FORK-CLEAN ( [ -- ] -- )
   MEMT-FORK PROCESS:STATUS>RC RC>N 0 T= ;

: RBT-RELEASED-BASE ( -- ^u8 )
   MEM-64K 2 * BYTES-ALLOC-LEN ALLOC-BYTES
   over {: base:^ :}
   MEMT-MARK-A base c!
   MEMT-MARK-Z base MEM-64K 2 * 1 - + c!
   RELEASE-BYTES
   base ;

: RBT-RELEASE-FIRST ( -- )
   RBT-RELEASED-BASE c@ drop ;

: RBT-RELEASE-LAST ( -- )
   RBT-RELEASED-BASE MEM-64K 2 * 1 - + c@ drop ;

: RBT-RELEASE-FIRST-SRC ( -- str )
   s" package MEMT RBT-RELEASE-FIRST ;package" ;

: RBT-RELEASE-LAST-SRC ( -- str )
   s" package MEMT RBT-RELEASE-LAST ;package" ;

\ Runs the source in a subject child and asserts its exit code, leaving the
\ stdout and stderr lengths.
: RBT-EXITS ( str n -- n n ) {: src:str want:n :}
   src RBT-OUT RBT-CAP >LEN RBT-ERR RBT-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN {: outu:len erru:len oc :}
   src RBT-OUT outu LEN>N >str RBT-ERR erru LEN>N >str
   oc want T-ENDING-EXITED=
   outu LEN>N erru LEN>N ;

\ A child that touches released memory exits 110 after the fault's line on fd 2.
: RBT-EXPECT-FAULT ( str -- )
   110 RBT-EXITS
   0 > TTRUE
   0 T= ;

: RBT-EXACT-SPANS ( -- )
   T-RESET
   RBT-RELEASE-FIRST-SRC RBT-EXPECT-FAULT
   RBT-RELEASE-LAST-SRC RBT-EXPECT-FAULT
   T-REPORT ;

: RBT-ACTION ( -- [ -- renders ] )
   [: RBT-EXACT-SPANS ;] ;

: TEST-ALLOC-ROLES ( -- )
   T-RESET
   MEMT-TYPED-BYTES
   MEMT-TYPED-64K
   MEMT-TYPED-CELLS
   T-REPORT ;

TEST-ALLOC-ROLES

false [if] \ off until dot habu-add-the-checks-a49d054d
: TEST-ALLOC-SIZES ( -- )
   T-RESET
   [: 0 BYTES-ALLOC-LEN drop ;] E-MEM-SIZE TTHROWSQ
   [: -1 BYTES-ALLOC-LEN drop ;] E-MEM-SIZE TTHROWSQ
   [: 0 CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   [: -1 CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   [: MEM-MAX-CELLS 1 + CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   T-REPORT ;

TEST-ALLOC-SIZES
[then]

RBT-ACTION

;package
;using

execute

\ Fatal syscall failures bypass catch and cannot print the survival byte.
package MEM-FATAL-TEST

$100 constant CAP

create CAP-OUT CAP allot
create CAP-ERR CAP allot

: EXPECT-FATAL ( str -- ) {: src:str :}
   src CAP-OUT CAP >LEN CAP-ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   src CAP-OUT outu LEN>N >str CAP-ERR erru LEN>N >str
   oc EXIT-STATUS:SYSTEM T-ENDING-EXITED=
   outu LEN>N 0 T=
   CAP-ERR erru LEN>N >str S\" memory: unmap failed\n" T$= ;   \ die ends the message with one newline

: RELEASE-SRC ( -- str )
   s" package MFR : RUN ( -- ) MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES swap 1 + swap MEM:RELEASE-BYTES ; ' RUN ;package catch $53 emit" ;

public

: RUN ( -- )
   T-RESET
   RELEASE-SRC EXPECT-FATAL
   T-REPORT ;

;package

MEM-FATAL-TEST:RUN

\ Child processes prove release through OS-enforced mapping accessibility.
using MEM
package MEMT
private
PTR-VARIABLE WBT-SAVED
PTR-VARIABLE WBT-OUTER
TASK:MIN-STACK TASK:TASK WBT-PEER
TASK:SEMAPHORE WBT-OWN-MARKED
variable WBT-PEER-IN

: WBT-PROBE-RELEASED ( -- )
   2 close
   WBT-SAVED @ c@ drop
   MEMT-EXIT0 ;

\ The child exits 110, the memory fault.
: WBT-RELEASED ( -- )
   [: WBT-PROBE-RELEASED ;] MEMT-FORK
   MATCH PROCESS:status
      exited OF 110 <> if E-PRIMARY throw then ENDOF
      signaled OF drop E-PRIMARY throw ENDOF
   ;MATCH ;

: WBT-RESULT-BODY ( ^u8 NUM:alloc-byte-len -- n )
   drop {: buf:^ :}
   buf WBT-SAVED !
   MEMT-MARK-A buf c!
   buf c@ ;

: WBT-THROW-BODY ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   buf WBT-SAVED !
   E-PRIMARY throw ;

: WBT-SAVE-INNER ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   buf WBT-SAVED !
   MEMT-MARK-Z buf c! ;

: WBT-TOUCH-INNER ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   MEMT-MARK-Z buf c!
   buf c@ MEMT-MARK-Z <> if E-PRIMARY throw then ;

: WBT-INNER-RELEASE ( ^u8 NUM:alloc-byte-len -- )
   drop {: outer:^ :}
   outer WBT-OUTER !
   MEMT-MARK-A outer c!
   MEM-64K BYTES-ALLOC-LEN [: WBT-SAVE-INNER ;] WITH-BYTES
   WBT-OUTER @ c@ MEMT-MARK-A <> if E-PRIMARY throw then
   WBT-RELEASED ;

: WBT-OUTER-RELEASE ( ^u8 NUM:alloc-byte-len -- )
   drop {: outer:^ :}
   outer WBT-SAVED !
   MEMT-MARK-A outer c!
   MEM-64K BYTES-ALLOC-LEN [: WBT-TOUCH-INNER ;] WITH-BYTES
   outer c@ MEMT-MARK-A <> if E-PRIMARY throw then ;

: WBT-RESULT ( -- n )
   MEM-64K BYTES-ALLOC-LEN [: WBT-RESULT-BODY ;] WITH-BYTES ;

: WBT-THROW ( -- )
   MEM-64K BYTES-ALLOC-LEN [: WBT-THROW-BODY ;] WITH-BYTES ;

\ A record this small is pooled, so its release shows as the next scope
\ taking the same body.
STRUCTURE wrt-pair DERIVE addr
   FIELD a n
   FIELD b n
;STRUCTURE
TYPED-VARIABLE WRT-SAVED ^wrt-pair

: WRT-ZERO ( ^wrt-pair -- )
   {: p:^ :}
   s" a lent record reads 0" T-LABEL
   p WRT-PAIR-A @ 0 T=
   p WRT-PAIR-B @ 0 T= ;

: WRT-FILL ( ^wrt-pair -- )
   {: p:^ :}
   p WRT-ZERO
   p WRT-SAVED !
   7 p WRT-PAIR-A !
   9 p WRT-PAIR-B !
   s" a lent record keeps what is written" T-LABEL
   p WRT-PAIR-A @ 7 T=
   p WRT-PAIR-B @ 9 T= ;

: WRT-SAME ( ^wrt-pair -- )
   {: p:^ :}
   s" a record scope reuses the body the last one left" T-LABEL
   p WRT-SAVED @ = TTRUE
   p WRT-ZERO ;

: WRT-THROW-BODY ( ^wrt-pair -- )
   WRT-FILL E-PRIMARY throw ;

: WRT-THROW ( -- )
   [type] wrt-pair [: WRT-THROW-BODY ;] WITH-RECORD ;

: WRT-REUSE ( -- )
   [type] wrt-pair [: WRT-FILL ;] WITH-RECORD
   [type] wrt-pair [: WRT-SAME ;] WITH-RECORD
   [: WRT-THROW ;] E-PRIMARY TTHROWSQ
   [type] wrt-pair [: WRT-SAME ;] WITH-RECORD ;

: WBT-NORMAL-CHILD ( -- )
   WBT-RESULT MEMT-MARK-A <> if E-PRIMARY throw then
   WBT-RELEASED
   MEMT-EXIT0 ;

: WBT-THROW-CHILD ( -- )
   [: WBT-THROW ;] catch E-PRIMARY <> if E-PRIMARY throw then
   WBT-RELEASED
   MEMT-EXIT0 ;

: WBT-INNER-CHILD ( -- )
   MEM-64K BYTES-ALLOC-LEN [: WBT-INNER-RELEASE ;] WITH-BYTES
   MEMT-EXIT0 ;

: WBT-OUTER-CHILD ( -- )
   MEM-64K BYTES-ALLOC-LEN [: WBT-OUTER-RELEASE ;] WITH-BYTES
   WBT-RELEASED
   MEMT-EXIT0 ;

: WBT-RETAINED-CHILD ( -- )
   MEM-64K BYTES-ALLOC-LEN ALLOC-BYTES
   drop {: buf:^ :}
   buf WBT-SAVED !
   MEMT-MARK-A buf c!
   WBT-PROBE-RELEASED ;

\ A size the scope header still fits beside, so the refusal is the OS mapping's.
: WBT-ALLOC-FAIL ( -- )
   MEM-MAX-N 2 / BYTES-ALLOC-LEN [: 2drop ;] WITH-BYTES ;


: WBT-ALLOC-RETRY ( ^u8 NUM:alloc-byte-len -- )
   drop {: outer:^ :}
   MEMT-MARK-A outer c!
   [: WBT-ALLOC-FAIL ;] E-MEM-MAP TTHROWSQ
   outer c@ MEMT-MARK-A T=
   WBT-RESULT MEMT-MARK-A T= ;

\ Scopes nest per task: a peer leaving its own scope leaves this task's open
\ mapping intact, where a process-wide scope stack would release it and fault.
: WBT-PEER-BODY ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   MEMT-MARK-Z buf c!
   1 WBT-PEER-IN atomic!
   WBT-OWN-MARKED TASK:WAIT-SEM
   buf c@ TASK:RETURN ;

: WBT-PEER-WORK ( -- )
   MEM-64K BYTES-ALLOC-LEN [: WBT-PEER-BODY ;] WITH-BYTES ;

: WBT-OWN-BODY ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   MEMT-MARK-A buf c!
   WBT-OWN-MARKED TASK:SIGNAL-SEM
   WBT-PEER TASK:JOIN MATCH result
      ok OF MEMT-MARK-Z <> if E-PRIMARY throw then ENDOF
      err OF drop E-PRIMARY throw ENDOF
   ;MATCH
   buf c@ MEMT-MARK-A <> if E-PRIMARY throw then ;

: WBT-TWO-TASK-CHILD ( -- )
   0 WBT-PEER-IN atomic!
   0 WBT-OWN-MARKED TASK:SEMAPHORE-INIT
   ['] WBT-PEER-WORK WBT-PEER TASK:ACTIVATE
   WBT-PEER-IN 1 WBT-PEER TEST-WAIT:FOR-CELL
   MEM-64K BYTES-ALLOC-LEN [: WBT-OWN-BODY ;] WITH-BYTES
   WBT-OWN-MARKED TASK:SEMAPHORE-DESTROY
   MEMT-EXIT0 ;


\ A task halted at TASK:PAUSE inside WITH-BYTES leaves the scope as a throw
\ does: the scope's cleanup releases the mapping, and the join answers
\ E-TASK-NO-RESULT because the halt is no body error. The task stores its
\ buffer and raises the ready cell before its PAUSE loop, so the HALT lands
\ inside the scope.
align
variable WBT-HALT-READY
TASK:MIN-STACK TASK:TASK WBT-HALT-TASK

: WBT-HALT-BODY ( ^u8 NUM:alloc-byte-len -- )
   drop WBT-SAVED !
   1 WBT-HALT-READY atomic!
   begin TASK:PAUSE again ;

: WBT-HALT-WORK ( -- )
   MEM-64K BYTES-ALLOC-LEN [: WBT-HALT-BODY ;] WITH-BYTES ;

: WBT-HALTED ( -- )
   0 WBT-HALT-READY atomic!
   ['] WBT-HALT-WORK WBT-HALT-TASK TASK:ACTIVATE
   WBT-HALT-READY 1 WBT-HALT-TASK TEST-WAIT:FOR-CELL
   WBT-HALT-TASK TASK:HALT
   s" a task halted inside WITH-BYTES ends without a result" T-LABEL
   WBT-HALT-TASK TASK:JOIN
   MATCH result
      ok OF drop E-PRIMARY throw ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   WBT-RELEASED ;

: TEST-WITH-BYTES ( -- )
   T-RESET
   [: WBT-NORMAL-CHILD ;] MEMT-FORK-CLEAN
   [: WBT-THROW-CHILD ;] MEMT-FORK-CLEAN
   [: WBT-INNER-CHILD ;] MEMT-FORK-CLEAN
   [: WBT-OUTER-CHILD ;] MEMT-FORK-CLEAN
   [: WBT-RETAINED-CHILD ;] MEMT-FORK-CLEAN
   [: WBT-TWO-TASK-CHILD ;] MEMT-FORK-CLEAN
   [: WBT-ALLOC-FAIL ;] E-MEM-MAP TTHROWSQ
   WBT-HALTED
   MEM-64K BYTES-ALLOC-LEN [: WBT-ALLOC-RETRY ;] WITH-BYTES
   WBT-RESULT MEMT-MARK-A T=
   WRT-REUSE
   T-REPORT ;

TEST-WITH-BYTES

\ A WBP-LEN body's slot is under POOL-MAX, so pooled; WBP-DEPTH of them nest
\ past the first chunk, which holds 240.
256 constant WBP-LEN
512 constant WBP-DEPTH
PTR-VARIABLE WBP-FIRST
PTR-VARIABLE WBP-LAST
variable WBP-THROW-DEEP
TASK:MIN-STACK TASK:TASK WBP-TASK

: WBP-FILL ( ^u8 n -- )
   {: a:^ c :}
   WBP-LEN 0 ?do c a i + c! loop ;

\ The count of body bytes that do not hold `c`.
: WBP-MISS ( ^u8 n -- n )
   {: a:^ c :}
   0 WBP-LEN 0 ?do a i + c@ c <> if 1 + then loop ;

\ A forked case exits nonzero when one of its assertions failed.
: WBP-EXIT ( -- )
   T-FAILURES 0 <> if s" pooled case failed" EXIT-STATUS:FAILURE die then
   MEMT-EXIT0 ;

: WBP-DIRTY ( ^u8 NUM:alloc-byte-len -- )
   drop MEMT-MARK-A WBP-FILL ;

: WBP-CLEAN ( ^u8 NUM:alloc-byte-len -- )
   drop
   s" a reentered pooled scope reads 0" T-LABEL
   0 WBP-MISS 0 T= ;

: WBP-REENTER ( -- )
   WBP-LEN BYTES-ALLOC-LEN [: WBP-DIRTY ;] WITH-BYTES
   WBP-LEN BYTES-ALLOC-LEN [: WBP-CLEAN ;] WITH-BYTES ;

: WBP-REENTER-OUTER ( ^u8 NUM:alloc-byte-len -- )
   drop {: outer:^ :}
   outer MEMT-MARK-Z WBP-FILL
   WBP-REENTER
   s" an outer pooled scope keeps its marks" T-LABEL
   outer MEMT-MARK-Z WBP-MISS 0 T= ;

: WBP-ZERO-CHILD ( -- )
   T-RESET
   WBP-REENTER
   WBP-LEN BYTES-ALLOC-LEN [: WBP-REENTER-OUTER ;] WITH-BYTES
   WBP-EXIT ;

: WBP-SAVE ( ^u8 NUM:alloc-byte-len -- )
   drop WBT-SAVED ! ;

: WBP-SAME ( ^u8 NUM:alloc-byte-len -- )
   drop WBT-SAVED @ = TTRUE ;

: WBP-THROW ( -- )
   WBP-LEN BYTES-ALLOC-LEN [: WBT-THROW-BODY ;] WITH-BYTES ;

: WBP-REUSE ( -- )
   WBP-LEN BYTES-ALLOC-LEN [: WBP-SAVE ;] WITH-BYTES
   s" a pooled scope reuses the body a returned one left" T-LABEL
   WBP-LEN BYTES-ALLOC-LEN [: WBP-SAME ;] WITH-BYTES
   [: WBP-THROW ;] E-PRIMARY TTHROWSQ
   s" a pooled scope reuses the body a thrown one left" T-LABEL
   WBP-LEN BYTES-ALLOC-LEN [: WBP-SAME ;] WITH-BYTES ;

: WBP-REUSE-CHILD ( -- )
   T-RESET
   WBP-REUSE
   WBP-LEN BYTES-ALLOC-LEN [: 2drop WBP-REUSE ;] WITH-BYTES
   WBP-EXIT ;

defer WBP-NEST ( n -- )

\ Never 0, so a level's bytes differ from a fresh body's and its neighbours'.
: WBP-LEVEL-MARK ( n -- n )
   251 mod 1 + ;

: WBP-LEVEL-BODY ( n ^u8 NUM:alloc-byte-len -- )
   drop {: lvl buf:^ :}
   lvl 1 = if buf WBP-FIRST ! then
   lvl WBP-DEPTH = if buf WBP-LAST ! then
   buf lvl WBP-LEVEL-MARK WBP-FILL
   lvl WBP-DEPTH < if
      lvl 1 + WBP-NEST
   else
      WBP-THROW-DEEP @ 0 <> if E-PRIMARY throw then
   then
   s" a nested pooled body keeps its bytes" T-LABEL
   buf lvl WBP-LEVEL-MARK WBP-MISS 0 T= ;

: WBP-LEVEL ( n -- )
   WBP-LEN BYTES-ALLOC-LEN [: WBP-LEVEL-BODY ;] WITH-BYTES ;

: WBP-DEEP ( -- )
   1 WBP-NEST ;

: WBP-DEEP-CHILD ( -- )
   T-RESET
   0 WBP-THROW-DEEP !
   WBP-DEEP
   WBP-EXIT ;

: WBP-FIRST-AGAIN ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   s" a pooled scope after an unwind reuses the first body" T-LABEL
   buf WBP-FIRST @ = TTRUE
   s" a pooled scope after an unwind reads 0" T-LABEL
   buf 0 WBP-MISS 0 T= ;

: WBP-UNWIND-CHILD ( -- )
   T-RESET
   1 WBP-THROW-DEEP !
   [: WBP-DEEP ;] E-PRIMARY TTHROWSQ
   WBP-LAST @ {: deepest:^ :}
   WBP-LEN BYTES-ALLOC-LEN [: WBP-FIRST-AGAIN ;] WITH-BYTES
   0 WBP-THROW-DEEP ! WBP-DEEP
   s" a re-nest reuses the chunks the unwind kept" T-LABEL
   WBP-LAST @ deepest = TTRUE
   WBP-EXIT ;

: WBP-HALT-WORK ( -- )
   WBP-LEN BYTES-ALLOC-LEN [: WBT-HALT-BODY ;] WITH-BYTES ;

: WBP-HALT-CHILD ( -- )
   T-RESET
   0 WBT-HALT-READY atomic!
   ['] WBP-HALT-WORK WBP-TASK TASK:ACTIVATE
   WBT-HALT-READY 1 WBP-TASK TEST-WAIT:FOR-CELL
   WBP-TASK TASK:HALT
   s" a task halted inside a pooled scope ends without a result" T-LABEL
   WBP-TASK TASK:JOIN
   MATCH result
      ok OF drop E-PRIMARY throw ENDOF
      err OF E-TASK-NO-RESULT T= ENDOF
   ;MATCH
   WBT-RELEASED
   WBP-EXIT ;

: WBP-JOIN-WORK ( -- )
   WBP-LEN BYTES-ALLOC-LEN [: WBP-SAVE ;] WITH-BYTES
   0 TASK:RETURN ;

: WBP-JOIN-CHILD ( -- )
   T-RESET
   ['] WBP-JOIN-WORK WBP-TASK TASK:ACTIVATE
   s" a task that ran a pooled scope joins with its answer" T-LABEL
   WBP-TASK TASK:JOIN
   MATCH result
      ok OF 0 T= ENDOF
      err OF drop E-PRIMARY throw ENDOF
   ;MATCH
   WBT-RELEASED
   WBP-EXIT ;

: WBP-PEER-BODY ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   buf MEMT-MARK-Z WBP-FILL
   1 WBT-PEER-IN atomic!
   WBT-OWN-MARKED TASK:WAIT-SEM
   buf MEMT-MARK-Z WBP-MISS TASK:RETURN ;

: WBP-PEER-WORK ( -- )
   WBP-LEN BYTES-ALLOC-LEN [: WBP-PEER-BODY ;] WITH-BYTES ;

: WBP-OWN-BODY ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   buf MEMT-MARK-A WBP-FILL
   WBT-OWN-MARKED TASK:SIGNAL-SEM
   s" a peer's pooled body keeps its marks" T-LABEL
   WBP-TASK TASK:JOIN
   MATCH result
      ok OF 0 T= ENDOF
      err OF drop E-PRIMARY throw ENDOF
   ;MATCH
   WBP-LEN BYTES-ALLOC-LEN [: WBP-REENTER-OUTER ;] WITH-BYTES
   s" a task's pooled body keeps its marks beside a peer" T-LABEL
   buf MEMT-MARK-A WBP-MISS 0 T= ;

: WBP-TWO-TASK-CHILD ( -- )
   T-RESET
   0 WBT-PEER-IN atomic!
   0 WBT-OWN-MARKED TASK:SEMAPHORE-INIT
   ['] WBP-PEER-WORK WBP-TASK TASK:ACTIVATE
   WBT-PEER-IN 1 WBP-TASK TEST-WAIT:FOR-CELL
   WBP-LEN BYTES-ALLOC-LEN [: WBP-OWN-BODY ;] WITH-BYTES
   WBT-OWN-MARKED TASK:SEMAPHORE-DESTROY
   WBP-EXIT ;

: WBP-MAX-16 ( -- )
   MEM-MAX-N 16 - BYTES-ALLOC-LEN [: 2drop ;] WITH-BYTES ;

: WBP-MAX-30 ( -- )
   MEM-MAX-N 30 - BYTES-ALLOC-LEN [: 2drop ;] WITH-BYTES ;

: WBP-MAX-CHILD ( -- )
   T-RESET
   s" a near-maximum scope is refused as a mapping" T-LABEL
   [: WBP-MAX-16 ;] E-MEM-MAP TTHROWSQ
   s" a near-maximum scope is refused as a mapping" T-LABEL
   [: WBP-MAX-30 ;] E-MEM-MAP TTHROWSQ
   WBP-REENTER
   WBP-EXIT ;

\ Neither MEMT-MARK-A nor MEMT-MARK-Z, so the pooled body around the mapped one
\ reads apart from both.
66 constant WBP-MARK-B

: WBP-MAPPED-BODY ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   buf MEMT-MARK-A WBP-FILL
   WBP-LEN BYTES-ALLOC-LEN [: WBP-REENTER-OUTER ;] WITH-BYTES
   s" a mapped body keeps its marks around a pooled scope" T-LABEL
   buf MEMT-MARK-A WBP-MISS 0 T= ;

: WBP-MIXED-OUTER ( ^u8 NUM:alloc-byte-len -- )
   drop {: buf:^ :}
   buf WBP-MARK-B WBP-FILL
   WBP-LEN BYTES-ALLOC-LEN [: WBP-SAVE ;] WITH-BYTES
   MEM-64K 16 + BYTES-ALLOC-LEN [: WBP-MAPPED-BODY ;] WITH-BYTES
   s" a pooled scope after a mapped one takes the slot before it" T-LABEL
   WBP-LEN BYTES-ALLOC-LEN [: WBP-SAME ;] WITH-BYTES
   s" a pooled body keeps its marks around a mapped scope" T-LABEL
   buf WBP-MARK-B WBP-MISS 0 T= ;

: WBP-MIXED-CHILD ( -- )
   T-RESET
   WBP-LEN BYTES-ALLOC-LEN [: WBP-MIXED-OUTER ;] WITH-BYTES
   WBP-REENTER
   WBP-EXIT ;

: TEST-POOLED-BYTES ( -- )
   T-RESET
   [: WBP-LEVEL ;] is WBP-NEST
   s" pooled 1: a body reads 0 on re-entry" T-LABEL
   [: WBP-ZERO-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 2: a scope reuses the body before it" T-LABEL
   [: WBP-REUSE-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 3: deep nesting keeps each body" T-LABEL
   [: WBP-DEEP-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 4: an unwind through chunks reuses the first body" T-LABEL
   [: WBP-UNWIND-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 5: a halt releases the body" T-LABEL
   [: WBP-HALT-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 6: a joined task releases the body" T-LABEL
   [: WBP-JOIN-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 7: two tasks keep separate bodies" T-LABEL
   [: WBP-TWO-TASK-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 8: a near-maximum size is refused" T-LABEL
   [: WBP-MAX-CHILD ;] MEMT-FORK-CLEAN
   s" pooled 9: a mapped scope between pooled ones keeps both" T-LABEL
   [: WBP-MIXED-CHILD ;] MEMT-FORK-CLEAN
   T-REPORT ;

TEST-POOLED-BYTES

;package
;using

\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type error), 1 uncheckable.
package MEMT
private

\ A released guarded stack must leave its neighbours mapped: the kernel's
\ granule puts the page-aligned base up to a page into the span, so a release
\ computed from the span reaches past its end into the next mapping (a task's
\ DATA region).
: GUARDED ( -- )
   T-RESET
   s" releasing a guarded stack leaves its neighbours mapped" T-LABEL
   STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED {: a:^ au:n :}
   STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED {: b:^ bu:n :}
   STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED {: c:^ cu:n :}
   a au MEM:RELEASE-GUARDED
   c cu MEM:RELEASE-GUARDED
   77 b CELL-VIEW !
   88 b bu 8 - + CELL-VIEW !
   b CELL-VIEW @ 77 T=
   b bu 8 - + CELL-VIEW @ 88 T=
   b bu MEM:RELEASE-GUARDED
   T-REPORT ;

false [if] \ off until dot habu-add-the-checks-a49d054d
: GUARDED-SIZE-REFUSAL ( -- )
   100 MEM:ALLOC-GUARDED MEM:RELEASE-GUARDED ;

: GUARDED-SIZE ( -- )
   T-RESET
   s" a guarded stack needs a whole-page capacity" T-LABEL
   ['] GUARDED-SIZE-REFUSAL catch E-MEM-SIZE T=
   T-REPORT ;

GUARDED-SIZE
[then]

: STAT ( -- )
   T-RESET
   s" G-ALLOC ( NUM:alloc-byte-len -- ^u8 NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-ALLOC-CELLS ( NUM:alloc-cell-count -- ^a ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-BYTES-ALLOC-LEN ( n -- NUM:alloc-byte-len ) MEM:BYTES-ALLOC-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-CELLS-ALLOC-COUNT ( n -- NUM:alloc-cell-count ) MEM:CELLS-ALLOC-COUNT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-ZEROABLE-ALLOC ( NUM:byte-len -- ^u8 NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-CELL-ROLE-BYTES ( NUM:alloc-cell-count -- ^u8 NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-BYTE-ROLE-CELLS ( NUM:alloc-byte-len -- ^a ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ RELEASE-BYTES admits only the ^u8 + alloc-byte-len ALLOC-BYTES mints, so no
   \ forged address or unvalidated size reaches munmap.
   s" G-RELEASE ( ^u8 NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-RELEASE-RAW-PTR ( n NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-RAW-LEN ( ^u8 n -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-ZEROABLE-LEN ( ^u8 NUM:byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-CELL-LEN ( ^u8 NUM:alloc-cell-count -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ WITH-BYTES types the body row, so a caller cannot scope a mapping over an
   \ unvalidated size or a byte/cell role swap.
   s" G-WITH-BYTES ( R NUM:alloc-byte-len [ R ^u8 NUM:alloc-byte-len -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-WB-RAW-LEN ( R n [ R ^u8 NUM:alloc-byte-len -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-WB-CELL-LEN ( R NUM:alloc-cell-count [ R ^u8 NUM:alloc-cell-count -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;

GUARDED
STAT

;package

s" memory-test: ok" type cr

\ gate-pool-test.f - focused coverage for fork-backed test pool workers.
\ Run: bin/hb --load test/gate-pool-test.f

require lib/errors.f
require lib/string.f
require lib/string-roles.f               \ package STR: the typed string surface
require lib/fmt.f
require lib/adt/option.f                 \ option<NUM:index> STR:INDEX-OF consumer (switchover wave A)
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/time-cpu.f                   \ a worker runs a measured amount of CPU time
require lib/test/runner.f
require test/gate-pool.f

package GATE-POOL-TEST
private

$20000 constant GPT-CAP
$2710 constant GPT-TIMEOUT-MS
250 constant GPT-HANG-TIMEOUT-MS
$100 constant GPT-BIG-CHUNK
400 constant GPT-BIG-N
GPT-BIG-CHUNK GPT-BIG-N * constant GPT-BIG-BYTES
160 constant GPT-SPEW-N
12 constant GPT-OVERFLOW-N

create GPT-OUT GPT-CAP allot
create GPT-ERR GPT-CAP allot
create GPT-BIG-BUF GPT-BIG-CHUNK allot
create GPT-ROOT-SAVE FS-PATH-CAP allot

variable GPT-COW
TYPED-VARIABLE GPT-BIG-A ptr u8
variable GPT-COUNT-N
variable GPT-ROOT-SAVE-U

: GPT-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: GPT-WORKER ( -- )
   7 GPT-COW !
   s" gate-pool fork worker" type cr ;

: GPT-BIG-FILL ( -- )
   GPT-BIG-CHUNK 0 ?do
      $41 GPT-BIG-BUF i + c!
   loop ;

: GPT-BIG-WORKER ( -- )
   GPT-BIG-FILL
   GPT-BIG-N 0 ?do
      GPT-BIG-BUF GPT-BIG-CHUNK type
   loop ;

: GPT-FAIL-SPEW ( -- )
   GPT-BIG-FILL
   GPT-SPEW-N 0 ?do
      GPT-BIG-BUF GPT-BIG-CHUNK type
   loop ;

: GPT-FAIL-WORKER ( -- )
   GPT-FAIL-SPEW
   s" gate-pool failing worker" type cr
   77 throw ;

: GPT-SOFT-ONE ( -- )
   s" gate-pool soft one" type cr
   77 throw ;

: GPT-SOFT-TWO ( -- )
   s" gate-pool soft two" type cr
   78 throw ;

: GPT-WRAP-WORKER ( -- )
   s" gate-pool wrap worker" type cr
   256 throw ;

\ Sentinel the hang worker writes as its final bytes just before spinning, so
\ the timeout battery can prove GT-POOL-TIMEOUT's final poll+drain captured the
\ last output the worker produced before it was killed.
: GPT-HANG-SENTINEL$ ( -- ptr u8 n )
   s" hang-sentinel-xyzzy" ;

: GPT-HANG-WORKER ( -- )
   s" gate-pool hang worker" type cr
   GPT-HANG-SENTINEL$ type
   begin 0 0= 0= until ;

: GPT-BATTERY-TRUNC ( -- )
   s" fork failing worker" GPT-TIMEOUT-MS [: GPT-FAIL-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-SOFT ( -- )
   s" soft fail one" GPT-TIMEOUT-MS [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   s" soft fail two" GPT-TIMEOUT-MS [: GPT-SOFT-TWO ;] GT-POOL-START-FORK
   s" soft pass" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-WRAP ( -- )
   s" wrap throw worker" GPT-TIMEOUT-MS [: GPT-WRAP-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-SPIN-WORKER ( -- )
   begin 0 0= 0= until ;

\ A row held to 1 ms of CPU is ended at the pool's first reading of it, and its
\ red line has to name its budget, never a load timeout.
: GPT-BATTERY-TIMEOUT ( -- )
   s" hang worker" GPT-HANG-TIMEOUT-MS [: GPT-HANG-WORKER ;] GT-POOL-START-FORK
   s" quick worker" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK
   s" budget worker" GPT-TIMEOUT-MS [: GPT-SPIN-WORKER ;] GT-POOL-START-FORK
   1 GT-POOL-SEQ @ GT-POOL-CPU-BUDGET!
   GT-POOL-DRAIN-SOFT ;

\ A suite's own inner deadline: lib/process throws E-PROC-TIMEOUT when a child
\ it spawned outlives the deadline it was given, and an uncaught one leaves the
\ suite through the engine's top-level reporter. The pool has to name that as
\ the load outcome it is (tools/imgdump-test.f reds this way under a saturated
\ pool), never as an anonymous exit.
: GPT-INNER-TIMEOUT-SRC$ ( -- ptr u8 n )
   S\" require lib/errors.f\nE-PROC-TIMEOUT throw\n" ;

\ The same shape with any other throw code stays a plain exit.
: GPT-OTHER-THROW-SRC$ ( -- ptr u8 n )
   S\" require lib/errors.f\nE-PROC-SPAWN throw\n" ;

: GPT-BATTERY-INNER-TIMEOUT ( -- )
   GPT-HB$ s" inner deadline worker" GPT-INNER-TIMEOUT-SRC$ GPT-TIMEOUT-MS GT-POOL-START-STDIN
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-OVERFLOW ( -- )
   GPT-OVERFLOW-N 0 ?do
      s" soft overflow" GPT-TIMEOUT-MS [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   loop ;

\ One child process runs every failure class; reds accumulate to 19 and the
\ final fail-closed drain lists every one of them.
: GPT-BATTERY-CASE ( -- )
   s" gate-pool-test-battery" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   GPT-BATTERY-TRUNC
   GPT-BATTERY-SOFT
   GPT-BATTERY-WRAP
   GPT-BATTERY-TIMEOUT
   GPT-BATTERY-INNER-TIMEOUT
   GPT-BATTERY-OVERFLOW
   GT-POOL-DRAIN ;

: GPT-MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   0 0= 0= ;

: GPT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: GPT-MODE-CAPTURE ( ptr u8 n -- n n n ) {: mode:ptr modeu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-pool-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   GPT-HB$ >LEN GPT-OUT GPT-CAP >LEN GPT-ERR GPT-CAP >LEN
   GPT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   GPT-CAPTURE>N ;

: GPT-BATTERY-CAPTURE ( -- n n n )
   s" fail-battery-case" GPT-MODE-CAPTURE ;

: GPT-COUNT$ ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n needle:ptr nu:n :}
   0 GPT-COUNT-N !
   nu 0 <= if 0 exit then
   0 begin dup nu + u <= while
      a over BYTE+ nu needle nu STR= if
         GPT-COUNT-N @ 1+ GPT-COUNT-N !
      then
      1+
   repeat drop
   GPT-COUNT-N @ ;

;package

package GATE-POOL-TEST

private

: GPT-EXPECT-TRUNC-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool failing worker" CONTAINS? TTRUE
   GPT-OUT outu s" [tail truncated " CONTAINS? TTRUE
   GPT-OUT outu s" outcome: exit" CONTAINS? TTRUE
   GPT-OUT outu s" fork worker throw rc 77" CONTAINS? TTRUE
   GPT-OUT outu s" code: 1" CONTAINS? TTRUE
   GPT-OUT outu s" stdout-file: " CONTAINS? TTRUE
   GPT-OUT outu s" stderr-file: " CONTAINS? TTRUE
   GPT-OUT outu s" -out.log" CONTAINS? TTRUE
   GPT-OUT outu s" RED: fork failing worker" CONTAINS? TTRUE
   GPT-OUT outu s" FAIL: fork failing worker" CONTAINS? TTRUE ;

: GPT-EXPECT-SOFT-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool soft one" CONTAINS? TTRUE
   GPT-OUT outu s" gate-pool soft two" CONTAINS? TTRUE
   GPT-OUT outu s" FAIL: soft fail one" CONTAINS? TTRUE
   GPT-OUT outu s" FAIL: soft fail two" CONTAINS? TTRUE
   GPT-OUT outu s" PASS: soft pass" CONTAINS? TTRUE
   GPT-OUT outu s" fork worker throw rc 78" CONTAINS? TTRUE
   GPT-OUT outu s" RED: soft fail one" CONTAINS? TTRUE
   GPT-OUT outu s" RED: soft fail two" CONTAINS? TTRUE
   GPT-OUT outu s" RED: soft pass" CONTAINS? TFALSE ;

: GPT-EXPECT-WRAP-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool wrap worker" CONTAINS? TTRUE
   GPT-OUT outu s" fork worker throw rc 256" CONTAINS? TTRUE
   GPT-OUT outu s" RED: wrap throw worker" CONTAINS? TTRUE ;

: GPT-EXPECT-TIMEOUT-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" gate-pool hang worker" CONTAINS? TTRUE
   GPT-OUT outu GPT-HANG-SENTINEL$ CONTAINS? TTRUE
   GPT-OUT outu s" outcome: TIMEOUT-UNDER-LOAD code: 0" CONTAINS? TTRUE
   GPT-OUT outu s" RED: hang worker kind=TIMEOUT-UNDER-LOAD code=0" CONTAINS? TTRUE
   GPT-OUT outu s" sat=" CONTAINS? TTRUE
   GPT-OUT outu s" waits=" CONTAINS? TTRUE
   GPT-OUT outu s" ran=" CONTAINS? TTRUE
   GPT-OUT outu s" PASS: quick worker" CONTAINS? TTRUE ;

: GPT-EXPECT-BUDGET-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" outcome: CPU-BUDGET code: 0" CONTAINS? TTRUE
   GPT-OUT outu s" RED: budget worker kind=CPU-BUDGET code=0" CONTAINS? TTRUE
   GPT-OUT outu s" /1ms" CONTAINS? TTRUE ;

\ The row the pool's own reaper never touched: the child reported its own
\ expired deadline and the pool read that report, so the report reads like the
\ row a pool-side kill produces and never kind=exit. The count keeps it from
\ passing on the hang worker's row alone; GPT-INNER-TIMEOUT-CASE holds the
\ saturation fields the suffix is rendered from.
: GPT-EXPECT-INNER-TIMEOUT-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" RED: inner deadline worker kind=TIMEOUT-UNDER-LOAD code=0" CONTAINS? TTRUE
   GPT-OUT outu s" RED: inner deadline worker kind=exit" CONTAINS? TFALSE
   GPT-OUT outu s" kind=TIMEOUT-UNDER-LOAD" GPT-COUNT$ 2 T=
   GPT-OUT outu s" outcome: TIMEOUT-UNDER-LOAD code: 0" CONTAINS? TTRUE ;

: GPT-EXPECT-OVERFLOW-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" red tests: 19" CONTAINS? TTRUE
   GPT-OUT outu s" more failed tests" CONTAINS? TFALSE
   GPT-OUT outu s" RED: soft overflow" GPT-COUNT$ 12 T= ;

: GPT-EXPECT-BATTERY-ERR ( n -- ) {: erru:n :}
   GPT-ERR erru s" test pool failed" CONTAINS? TTRUE ;

: GPT-BATTERY-REPORT ( -- )
   GPT-BATTERY-CAPTURE 1 T=
   {: outu:n erru:n :}
   outu GPT-EXPECT-TRUNC-OUT
   outu GPT-EXPECT-SOFT-OUT
   outu GPT-EXPECT-WRAP-OUT
   outu GPT-EXPECT-TIMEOUT-OUT
   outu GPT-EXPECT-BUDGET-OUT
   outu GPT-EXPECT-INNER-TIMEOUT-OUT
   outu GPT-EXPECT-OVERFLOW-OUT
   erru GPT-EXPECT-BATTERY-ERR ;

: GPT-BIG-ALLOC ( -- )
   GPT-BIG-A @ 0= if
      GPT-BIG-BYTES $400 + GT-POOL-ALLOC-BYTES GPT-BIG-A !
   then ;

: GPT-BIG-FILE-LEN ( -- n )
   0 >IDX GT-POOL-OUT-FILE$ GPT-BIG-A @ GPT-BIG-BYTES $400 + READ-ALL ;

: GPT-BIG-EXPECT ( -- )
   0 >IDX GT-POOL-OUT-U-PTR @ GT-OUT-CAP T=
   0 >IDX GT-POOL-OUT-TOTAL-PTR @ GPT-BIG-BYTES T=
   0 >IDX GT-POOL-OUT-FILE$ FILE? TTRUE
   GPT-BIG-FILE-LEN GPT-BIG-BYTES T=
   GPT-BIG-A @ c@ $41 T=
   GPT-BIG-A @ GPT-BIG-BYTES 1- BYTE+ c@ $41 T=
   0 >IDX GT-POOL-OUT-BUF c@ $41 T= ;

: GPT-BIG-CASE ( -- )
   GPT-BIG-ALLOC
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork big output" GPT-TIMEOUT-MS [: GPT-BIG-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GPT-BIG-EXPECT ;

: GPT-ROOT-SAVE! ( -- )
   GT-ROOT {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GPT-ROOT-SAVE u BYTE-COPY
   u GPT-ROOT-SAVE-U ! ;

: GPT-ROOT-RESTORE ( -- )
   GPT-ROOT-SAVE GPT-ROOT-SAVE-U @ GT-COPY-ROOT! ;

: GPT-INFRA-START ( -- )
   s" infra fork" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK ;

: GPT-INFRA-CASE ( -- )
   GPT-ROOT-SAVE!
   s" /nonexistent-habu-pool-root" GT-COPY-ROOT!
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   [: GPT-INFRA-START ;] E-FS-OPEN TTHROWSQ
   GT-POOL-RED# 0 T=
   GT-POOL-LIVE @ 0 T=
   GPT-ROOT-RESTORE ;

\ Group-kill regression: a fork worker forks a grandchild that would write a
\ sentinel after a delay, then hangs. The pool times the worker out and
\ GT-POOL-KILL-SLOT signals the whole process group, so the grandchild dies
\ before its delay elapses and the sentinel never appears.
\ The case waits for that death itself, not for a fixed settle: the worker, its
\ reaper and the grandchild all inherit the write end of a pipe the case opens
\ before the fork and closes in itself, so the read end reports EOF once the
\ last of them is gone. A grandchild the kill missed holds it open until it has
\ written the sentinel, DELAY after it started; one that never goes fails the
\ GONE deadline.
250 constant GPT-GK-TIMEOUT-MS
1500 constant GPT-GK-DELAY-MS
3000 constant GPT-GK-GONE-MS       \ past DELAY, so a grandchild the kill missed has written by then

create GPT-GK-SENTINEL FS-PATH-CAP allot
create GPT-GK-SLEEP-PFD 64 allot
variable GPT-GK-SENTINEL-U

: GPT-GK-SENTINEL$ ( -- ptr u8 n )
   GPT-GK-SENTINEL GPT-GK-SENTINEL-U @ ;

\ Wall-clock sleep by polling no fds until the deadline; poll rc (timeout or
\ EINTR) is intentionally ignored because both just re-loop toward the deadline.
: GPT-SLEEP-MS ( n -- ) {: ms:n :}
   mono-ns ms PROC-NS-PER-MS * + {: deadline:n :}
   begin mono-ns deadline < while
      GPT-GK-SLEEP-PFD 0 50 poll drop
   repeat ;

: GPT-GK-GRANDCHILD ( -- )
   GPT-GK-DELAY-MS GPT-SLEEP-MS
   GPT-GK-SENTINEL$ s" grandchild-was-here" WRITE-ALL
   s" " 0 die ;

: GPT-GK-CHILD ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if GPT-GK-GRANDCHILD then
   s" gate-pool group-kill child" type cr
   begin 0 0= 0= until ;

\ True once every holder of the pipe's write end has exited.
: GPT-GK-GONE? ( fd -- bool ) {: rd:fd :}
   rd POLLIN PROC-PFD!
   1 GPT-GK-GONE-MS GPT-GK-GONE-MS >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

: GPT-GK-SENTINEL! ( -- )
   GT-ROOT s" group-kill-sentinel" GPT-GK-SENTINEL JOIN-PATH GPT-GK-SENTINEL-U ! ;

: GPT-GROUP-KILL-CASE ( -- )
   s" gate-pool-group-kill" GT-START
   GPT-GK-SENTINEL!
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   PIPE-PAIR {: rd:fd wr:fd :}
   s" group-kill worker" GPT-GK-TIMEOUT-MS [: GPT-GK-CHILD ;] GT-POOL-START-FORK
   wr FD>N close
   GT-POOL-DRAIN-SOFT
   s" group-kill: the worker and everything it started are gone" T-LABEL
   rd GPT-GK-GONE? TTRUE
   rd FD>N close
   s" group-kill: grandchild sentinel absent" T-LABEL
   GPT-GK-SENTINEL$ EXISTS? TFALSE
   GT-CLEANUP ;

\ Regression for the parent-death reaper's wait(-1) conflict: the reaper armed
\ by GT-POOL-FORK-CHILD must be reparented away from the worker (double-fork),
\ so a worker body that calls wait(-1) expecting no children of its own still
\ gets ECHILD instead of blocking forever on the reaper. This is exactly the
\ case that stalled stdlib/tail-process (lib/process-test.f TEST-WAIT-BAD) when
\ the reaper was a direct child. If the reaper regresses to a worker child, the
\ worker blocks, the pool times it out, and GT-POOL-DRAIN dies here.
: GPT-WAIT-ANY ( -- )
   -1 >PID PROC-WAIT-RC MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ;

: GPT-WAIT-NEG-WORKER ( -- )
   T-RESET
   [: GPT-WAIT-ANY ;] E-PROC-WAIT TTHROWSQ
   T-REPORT ;

: GPT-WAIT-NEG-CASE ( -- )
   s" gate-pool-wait-neg" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" wait-neg worker" GPT-TIMEOUT-MS [: GPT-WAIT-NEG-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GT-CLEANUP ;

\ A child that dies by a SIGKILL the pool did NOT send: it signals its own
\ process group (an OOM/AMFI kill is the real-world analog). It reaps as
\ signaled(9) - rc 137 when flattened - but the pool's timed-out flag stays
\ false, so it is a plain FAIL (kind=signal), never TIMEOUT-UNDER-LOAD. waitpid's
\ WIFSIGNALED vs WIFEXITED (PROC-STATUS>OUTCOME) also keeps a self-exit(137)
\ distinct: that would read exited(137)/kind=exit, likewise not a timeout.
: GPT-EXTERNAL-KILL-WORKER ( -- )
   s" gate-pool external-kill worker" type cr
   0 >PID SIGKILL PROC-KILL-RAW drop ;

: GPT-EXTERNAL-KILL-CASE ( -- )
   s" gate-pool-external-kill" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" external-kill worker" GPT-TIMEOUT-MS [: GPT-EXTERNAL-KILL-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   s" external-kill: pool-unsent SIGKILL stays plain FAIL, not a timeout" T-LABEL
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-TIMED-OUT-PTR @ TFALSE
   0 GT-POOL-RED-EXITED-PTR @ TFALSE
   0 GT-POOL-RED-CODE-PTR @ SIGKILL T=
   GT-CLEANUP ;

\ Fork-inherited cleanup registrations. lib/fs-mutate.f's cleanup table records
\ the paths THIS process owns and CLEANUP-RUN deletes every entry in it, so a
\ fork child that inherits the parent's entries and runs its own cleanups
\ deletes paths the live parent still owns. In the gate that was the driver's
\ whole capture root, removed out from under its running siblings (E-FS-OPEN /
\ E-FS-IO on a rotating victim). PROC-FORK:RAW empties the table in the child,
\ so a child cleans up exactly what the child registered.
\
\ The evidence lives in a private temp tree that is deliberately NOT registered
\ for cleanup: an unfixed child also runs the inherited GT-ROOT entry, and
\ evidence kept under GT-ROOT would be erased by the very bug it records.
create GPT-FC-EV FS-PATH-CAP allot        \ evidence root; never registered
create GPT-FC-PARENT FS-PATH-CAP allot    \ registered by the parent, before the fork
create GPT-FC-PARENT-FILE FS-PATH-CAP allot
create GPT-FC-CHILD FS-PATH-CAP allot     \ registered by the child, after the fork
create GPT-FC-CHILD-FILE FS-PATH-CAP allot
create GPT-FC-DEPTH FS-PATH-CAP allot     \ one byte: the table depth the child inherited
create GPT-FC-DONE FS-PATH-CAP allot      \ written after the child's CLEANUP-RUN returns
create GPT-FC-BYTE 8 allot                \ a whole cell, so what follows stays aligned
variable GPT-FC-EV-U
variable GPT-FC-PARENT-U
variable GPT-FC-PARENT-FILE-U
variable GPT-FC-CHILD-U
variable GPT-FC-CHILD-FILE-U
variable GPT-FC-DEPTH-U
variable GPT-FC-DONE-U

: GPT-FC-EV$ ( -- ptr u8 n )
   GPT-FC-EV GPT-FC-EV-U @ ;

: GPT-FC-PARENT$ ( -- ptr u8 n )
   GPT-FC-PARENT GPT-FC-PARENT-U @ ;

: GPT-FC-PARENT-FILE$ ( -- ptr u8 n )
   GPT-FC-PARENT-FILE GPT-FC-PARENT-FILE-U @ ;

: GPT-FC-CHILD$ ( -- ptr u8 n )
   GPT-FC-CHILD GPT-FC-CHILD-U @ ;

: GPT-FC-CHILD-FILE$ ( -- ptr u8 n )
   GPT-FC-CHILD-FILE GPT-FC-CHILD-FILE-U @ ;

: GPT-FC-DEPTH$ ( -- ptr u8 n )
   GPT-FC-DEPTH GPT-FC-DEPTH-U @ ;

: GPT-FC-DONE$ ( -- ptr u8 n )
   GPT-FC-DONE GPT-FC-DONE-U @ ;

: GPT-FC-EV! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GPT-FC-EV u BYTE-COPY
   u GPT-FC-EV-U ! ;

: GPT-FC-SUB! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: base:ptr baseu:n name:ptr nameu:n dst:ptr up:ptr :}
   base baseu name nameu dst JOIN-PATH up ! ;

: GPT-FC-PATHS! ( -- )
   s" hb-fork-cleanup" HB-TMP-MKDIR GPT-FC-EV!
   GPT-FC-EV$ s" parent" GPT-FC-PARENT GPT-FC-PARENT-U GPT-FC-SUB!
   GPT-FC-PARENT$ s" keep" GPT-FC-PARENT-FILE GPT-FC-PARENT-FILE-U GPT-FC-SUB!
   GPT-FC-EV$ s" child" GPT-FC-CHILD GPT-FC-CHILD-U GPT-FC-SUB!
   GPT-FC-CHILD$ s" mark" GPT-FC-CHILD-FILE GPT-FC-CHILD-FILE-U GPT-FC-SUB!
   GPT-FC-EV$ s" depth" GPT-FC-DEPTH GPT-FC-DEPTH-U GPT-FC-SUB!
   GPT-FC-EV$ s" done" GPT-FC-DONE GPT-FC-DONE-U GPT-FC-SUB! ;

: GPT-FC-REGISTER-PARENT ( -- )
   GPT-FC-PARENT$ MAKE-DIRS
   GPT-FC-PARENT-FILE$ s" keep" WRITE-ALL
   GPT-FC-PARENT$ CLEANUP-TREE+ ;

\ The fork worker records the cleanup depth it inherited - one byte, written
\ before it touches the table - then registers and runs its OWN cleanup. It
\ asserts nothing: a failed assertion in a fork child still exits 0, so every
\ verdict belongs to the parent.
: GPT-FC-WORKER ( -- )
   FS-MUT-CLEANUP-N @ {: depth:n :}
   depth 0 < if E-FS-CAPACITY throw then
   depth FS-MUT-CLEANUP-MAX > if E-FS-CAPACITY throw then
   depth GPT-FC-BYTE c!
   GPT-FC-DEPTH$ GPT-FC-BYTE 1 WRITE-ALL
   GPT-FC-CHILD$ MAKE-DIRS
   GPT-FC-CHILD-FILE$ s" mark" WRITE-ALL
   GPT-FC-CHILD$ CLEANUP-TREE+
   CLEANUP-RUN
   GPT-FC-DONE$ s" done" WRITE-ALL ;

\ Poison the byte before reading it, so a short or empty read cannot pass for a
\ recorded depth of 0, and demand exactly the one byte the child wrote.
: GPT-FC-DEPTH@ ( -- n )
   $FF GPT-FC-BYTE c!
   GPT-FC-DEPTH$ GPT-FC-BYTE 1 READ-ALL {: got:n :}
   got 1 <> if E-FS-IO throw then
   GPT-FC-BYTE c@ ;

: GPT-FC-CASE ( -- )
   s" gate-pool-fork-cleanup" GT-START     \ a registered capture root: the driver's own shape
   GPT-FC-PATHS!
   GPT-FC-REGISTER-PARENT
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" fork cleanup worker" GPT-TIMEOUT-MS [: GPT-FC-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   s" fork cleanup: the child inherits an empty cleanup table" T-LABEL
   GPT-FC-DEPTH@ 0 T=
   s" fork cleanup: the child ran its own cleanup" T-LABEL
   GPT-FC-CHILD$ EXISTS? TFALSE
   s" fork cleanup: the child ran to completion" T-LABEL
   GPT-FC-DONE$ FILE? TTRUE
   s" fork cleanup: the parent's directory survives the child" T-LABEL
   GPT-FC-PARENT$ DIR? TTRUE
   s" fork cleanup: the parent's file survives the child" T-LABEL
   GPT-FC-PARENT-FILE$ FILE? TTRUE
   GT-CLEANUP
   s" fork cleanup: the parent's own run removes what it registered" T-LABEL
   GPT-FC-PARENT$ EXISTS? TFALSE
   GPT-FC-EV$ REMOVE-TREE ;

\ A stdin-fed slot: the child engine reads its source from the pool's pipe,
\ and its output reaches the slot's capture like any spawned child's.
: GPT-STDIN-SRC$ ( -- ptr u8 n )
   S\" s\" gate-pool stdin worker\" type cr\n" ;

: GPT-STDIN-CASE ( -- )
   s" gate-pool-stdin" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   GPT-HB$ s" stdin worker" GPT-STDIN-SRC$ GPT-TIMEOUT-MS GT-POOL-START-STDIN
   GT-POOL-DRAIN-SOFT
   s" stdin: a stdin-fed slot runs the child on the piped source" T-LABEL
   GT-POOL-RED# 0 T=
   0 >IDX GT-POOL-OUT-BUF 0 >IDX GT-POOL-OUT-U-PTR @ s" gate-pool stdin worker" CONTAINS? TTRUE
   GT-CLEANUP ;

\ A SPAWNED SLOT WHOSE REAPER CANNOT FORK ENDS THE POOL. PROC-FORK:SPAWN-REAPER
\ throws when its fork fails, and GT-POOL-ARM-SPAWN-REAPER then kills every
\ slot, the child just spawned included, before E-PROC-SPAWN goes on:
\ otherwise that child would outlive a killed pool. PROC-FORK:FORK-CALL refuses
\ RAW's fork alone, answering Linux's -EAGAIN; the slot's spawn is the spawn
\ primitive, so the one refusal counted is the reaper's, after its child exists.
variable GPT-RR-REFUSALS

: GPT-RR-REFUSE ( -- n )
   1 GPT-RR-REFUSALS +!
   -11 ;

: GPT-RR-START ( -- )
   PROC-ARGV-RESET
   s" 30" >LEN PROC-ARGV+
   s" /bin/sleep" s" reaper refused" GPT-TIMEOUT-MS GT-POOL-START ;

: GPT-REAPER-REFUSED-CASE ( -- )
   s" gate-pool-reaper-refused" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   0 GPT-RR-REFUSALS !
   [: GPT-RR-REFUSE ;] is PROC-FORK:FORK-CALL
   s" reaper refused: a spawned slot whose reaper cannot fork throws E-PROC-SPAWN" T-LABEL
   [: GPT-RR-START ;] E-PROC-SPAWN TTHROWSQ
   PROC-FORK:FORK-CALL-DEFAULT
   GPT-RR-REFUSALS @ 1 T=
   s" reaper refused: the pool killed and reaped the slot's child" T-LABEL
   0 >IDX GT-POOL-PID@ PID>N -1 T=
   GT-CLEANUP ;

\ A FORKED WORKER WHOSE REAPER CANNOT FORK ENDS AS A FAILED ROW. The worker
\ arms its reaper before its body; when PROC-FORK:FORK-REAPER's intermediate
\ cannot fork the reaper, the worker throws E-PROC-SPAWN and must end on it.
\ Unwound instead, it returns into its copy of the driver's control path and
\ runs on as a second driver beside the real one. PROC-FORK:FORK-CALL refuses
\ forks only in the third generation - the driver is generation 0, the worker
\ 1, the intermediate 2 - so the driver's fork of the worker and the worker's
\ fork of the intermediate are real. The refusal is installed only while the
\ driver forks that worker, so the sibling row started before it arms normally.
variable GPT-RF-GEN
variable GPT-RF-DRIVER
variable GPT-RF-SIB-PID
variable GPT-RF-VIC-PID

: GPT-RF-FORK ( -- n )
   GPT-RF-GEN @ 2 >= if -11 exit then
   fork dup 0= if 1 GPT-RF-GEN +! then ;

: GPT-RF-BODY$ ( -- ptr u8 n )
   s" reaper-fork refused worker body" ;

: GPT-RF-VICTIM ( -- )
   GPT-RF-BODY$ type cr ;

: GPT-RF-SIBLING ( -- )
   s" reaper-fork sibling worker" type cr ;

\ What GT-POOL-FORK-THROW reports for E-PROC-SPAWN on the worker's stderr.
: GPT-RF-WANT$ ( -- ptr u8 n )
   SB-RESET
   s" fork worker throw rc " SB-APPEND
   E-PROC-SPAWN FMT:SB-INT
   SB$ ;

\ True once the process group pgid names is empty: a worker leads its own
\ group, and its intermediate and reaper stay in it. The sibling's reaper
\ leaves on its own once its worker exits, so this waits a bounded time.
: GPT-RF-GONE? ( n -- bool ) {: pgid:n :}
   mono-ns GPT-TIMEOUT-MS PROC-NS-PER-MS * + {: deadline:n :}
   begin
      pgid negate 0 kill-errno ESRCH# negate = if true exit then
      mono-ns deadline <
   while
      20 GPT-SLEEP-MS
   repeat
   false ;

: GPT-RF-START ( -- )
   s" reaper-fork sibling" GPT-TIMEOUT-MS [: GPT-RF-SIBLING ;] GT-POOL-START-FORK
   0 >IDX GT-POOL-PID@ PID>N GPT-RF-SIB-PID !
   0 GPT-RF-GEN !
   [: GPT-RF-FORK ;] is PROC-FORK:FORK-CALL
   s" reaper-fork refused" GPT-TIMEOUT-MS [: GPT-RF-VICTIM ;] GT-POOL-START-FORK
   1 >IDX GT-POOL-PID@ PID>N GPT-RF-VIC-PID ! ;

: GPT-REAPER-FORK-CASE ( -- )
   s" gate-pool-reaper-fork" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   getpid GPT-RF-DRIVER !
   [: GPT-RF-START ;] catch {: code:n :}
   PROC-FORK:FORK-CALL-DEFAULT
   getpid GPT-RF-DRIVER @ <> if
      s" reaper fork: a forked worker returned into the driver's control path" 3 die
   then
   s" reaper fork: the driver started both rows" T-LABEL
   code 0 T=
   GT-POOL-DRAIN-SOFT
   s" reaper fork: only the refused worker's row is red" T-LABEL
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-LABEL$ s" reaper-fork refused" T$=
   0 GT-POOL-RED-EXITED-PTR @ TTRUE
   s" reaper fork: the row reports E-PROC-SPAWN's code" T-LABEL
   1 >IDX GT-POOL-ERR-BUF 1 >IDX GT-POOL-ERR-U-PTR @ GPT-RF-WANT$ CONTAINS? TTRUE
   s" reaper fork: the refused worker's body never ran" T-LABEL
   1 >IDX GT-POOL-OUT-BUF 1 >IDX GT-POOL-OUT-U-PTR @ GPT-RF-BODY$ CONTAINS? TFALSE
   s" reaper fork: the sibling row ran unharmed" T-LABEL
   0 >IDX GT-POOL-OUT-BUF 0 >IDX GT-POOL-OUT-U-PTR @ s" reaper-fork sibling worker" CONTAINS? TTRUE
   s" reaper fork: no process the case started remains" T-LABEL
   GPT-RF-SIB-PID @ GPT-RF-GONE? TTRUE
   GPT-RF-VIC-PID @ GPT-RF-GONE? TTRUE
   GT-CLEANUP ;

\ THE POOL OWNS ITS SPAWNED CHILDREN'S SCRATCH (gate-pool.f GT-POOL-CHILD-TMP!).
\ Each spawned child is given a directory of its own as HB_TMP, so the tree it
\ makes the ordinary way - lib/fs-mutate.f HB-TMP-MKDIR - lands inside it, and
\ the pool removes that directory when the slot retires. The killed child is
\ the case nothing else covers: it never reaches a cleanup of its own, so
\ before this the tree it made outlived every run.
\
\ Each child prints the path it made; the slot's capture is how the parent
\ learns it, exactly as the timeout battery reads the hang sentinel. The hung
\ child's deadline is the whole of its run, so it is only as long as a loaded
\ host needs to boot the child and print: some 70 ms alone.
1000 constant GPT-CT-HANG-MS

: GPT-CT-GREEN-SRC$ ( -- ptr u8 n )
   S\" require lib/fs-mutate.f\n: W ( -- ) s\" gpt-child\" HB-TMP-MKDIR type cr ;\nW\n" ;

: GPT-CT-HANG-SRC$ ( -- ptr u8 n )
   S\" require lib/fs-mutate.f\n: W ( -- ) s\" gpt-child\" HB-TMP-MKDIR type cr begin 0 0= 0= until ;\nW\n" ;

: GPT-CT-PRINTED$ ( -- ptr u8 n )        \ slot 0's captured line, without its newline
   0 >IDX GT-POOL-OUT-BUF 0 >IDX GT-POOL-OUT-U-PTR @ {: a:ptr u:n :}
   u 0 > if
      a u 1- BYTE+ c@ STR-LF = if a u 1- exit then
   then
   a u ;

\ The environment is inherited first, HB_TMP and all: the pool's own row has to
\ REPLACE an inherited one, not queue up behind it, or the child resolves the
\ caller's HB_TMP and makes its tree outside the slot's directory. A row the
\ caller set to a value of its own is the opposite case and stays; that one is
\ checked where it is needed, test/nf-path-test.f (a 97-byte root reaches the
\ build unchanged).
: GPT-CT-RUN ( ptr u8 n ptr u8 n n -- ) {: src:ptr srcu:n label:ptr labelu:n ms:n :}
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   GPT-HB$ label labelu src srcu ms GT-POOL-START-STDIN
   GT-POOL-DRAIN-SOFT ;

: GPT-CT-EXPECT ( -- )
   s" child tmp: the slot was given a scratch directory" T-LABEL
   0 >IDX GT-POOL-TMP-PATH-U-PTR @ 0 > TTRUE
   s" child tmp: the child made its tree under that directory" T-LABEL
   GPT-CT-PRINTED$ 0 >IDX GT-POOL-TMP$ STARTS-WITH? TTRUE
   s" child tmp: the child's tree is gone after the drain" T-LABEL
   GPT-CT-PRINTED$ EXISTS? TFALSE
   s" child tmp: the slot's scratch directory is gone too" T-LABEL
   0 >IDX GT-POOL-TMP$ EXISTS? TFALSE ;

: GPT-CHILD-TMP-CASE ( -- )
   s" gate-pool-child-tmp" GT-START
   GPT-CT-HANG-SRC$ s" child tmp kill worker" GPT-CT-HANG-MS GPT-CT-RUN
   s" child tmp: the hung child died on its slot deadline" T-LABEL
   0 >IDX GT-POOL-TIMED-OUT-PTR @ TTRUE
   GPT-CT-EXPECT
   GPT-CT-GREEN-SRC$ s" child tmp green worker" GPT-TIMEOUT-MS GPT-CT-RUN
   s" child tmp: the green child exited clean" T-LABEL
   GT-POOL-RED# 0 T=
   GPT-CT-EXPECT
   GT-CLEANUP ;

\ A SCRATCH THAT WILL NOT GO FAILS ITS ROW, NOT THE POOL (gate-pool.f
\ GT-POOL-CHILD-TMP-REMOVE). A row can leave a tree under its HB_TMP deeper
\ than FS-PATH-CAP, which REMOVE-TREE refuses with E-FS-CAPACITY. The refusal
\ is named with the row, the path and the code, the row is red, and the pool
\ goes on to the next row; the run's own cleanup meets the same tree under the
\ root and is named too, before the red report's exit. The case runs in a
\ child (mode deep-scratch-case), so that exit is the child's.
\
\ The tree is made from short paths: round r makes dir/<r c's>/<segment> and
\ moves round r-1's top inside it, so the tree deepens by a round while no
\ call names more than a round's bytes. Each round adds at least
\ GPT-DS-SEG + 3 bytes, so GPT-DS-ROUNDS of them pass FS-PATH-CAP below any
\ directory.
120 constant GPT-DS-SEG
FS-PATH-CAP GPT-DS-SEG 3 + / 1+ constant GPT-DS-ROUNDS

create GPT-DS-SEG-BUF GPT-DS-SEG allot
create GPT-DS-NAME-BUF GPT-DS-ROUNDS allot
create GPT-DS-TOP FS-PATH-CAP allot      \ dir/<round's name>
create GPT-DS-MID FS-PATH-CAP allot      \ dir/<round's name>/<segment>
create GPT-DS-IN FS-PATH-CAP allot       \ the previous round's top, moved in
create GPT-DS-PREV FS-PATH-CAP allot     \ the previous round's top, in dir

: GPT-DS-SEG$ ( -- ptr u8 n )
   GPT-DS-SEG 0 ?do 100 GPT-DS-SEG-BUF i + c! loop
   GPT-DS-SEG-BUF GPT-DS-SEG ;

\ Round r's name: r bytes of c.
: GPT-DS-NAME$ ( n -- ptr u8 n ) {: r:n :}
   r 0 ?do 99 GPT-DS-NAME-BUF i + c! loop
   GPT-DS-NAME-BUF r ;

\ dir/<round r's name> into dst.
: GPT-DS-TOP-PATH ( ptr u8 n n ptr u8 -- n ) {: dir:ptr diru:n r:n dst:ptr :}
   dir diru r GPT-DS-NAME$ dst JOIN-PATH ;

\ dir/<round r's name>/<segment> into GPT-DS-MID.
: GPT-DS-MID-PATH ( ptr u8 n n -- n ) {: dir:ptr diru:n r:n :}
   dir diru r GPT-DS-TOP GPT-DS-TOP-PATH {: topu:n :}
   GPT-DS-TOP topu GPT-DS-SEG$ GPT-DS-MID JOIN-PATH ;

\ Round r's move, r above 1: where round r-1's top is in dir, and where it
\ goes inside round r's segment.
: GPT-DS-MOVE ( ptr u8 n n -- ptr u8 n ptr u8 n ) {: dir:ptr diru:n r:n :}
   dir diru r GPT-DS-MID-PATH {: midu:n :}
   GPT-DS-MID midu r 1- GPT-DS-IN GPT-DS-TOP-PATH {: inu:n :}
   dir diru r 1- GPT-DS-PREV GPT-DS-TOP-PATH {: prevu:n :}
   GPT-DS-PREV prevu GPT-DS-IN inu ;

: GPT-DS-ROUND ( ptr u8 n n -- ) {: dir:ptr diru:n r:n :}
   dir diru r GPT-DS-MID-PATH {: midu:n :}
   GPT-DS-MID midu MAKE-DIRS
   r 1 > if dir diru r GPT-DS-MOVE RENAME-FILE then ;

: GPT-DS-UNROUND ( ptr u8 n n -- )
   GPT-DS-MOVE {: from:ptr fromu:n to:ptr tou:n :}
   to tou from fromu RENAME-FILE ;

: GPT-DS-MAKE ( ptr u8 n -- ) {: dir:ptr diru:n :}
   GPT-DS-ROUNDS 1+ 1 ?do dir diru i GPT-DS-ROUND loop ;

\ The rounds undone from the last, so every directory is shallow again.
: GPT-DS-UNDO ( ptr u8 n -- ) {: dir:ptr diru:n :}
   2 GPT-DS-ROUNDS ?do dir diru i GPT-DS-UNROUND -1 +loop ;

: GPT-DS-CASE ( -- )
   s" gate-pool-deep-scratch" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/true" s" deep scratch row" GPT-TIMEOUT-MS GT-POOL-START
   0 >IDX GT-POOL-TMP$ GPT-DS-MAKE
   s" deep slot: " type 0 >IDX GT-POOL-TMP$ type cr
   s" deep root: " type GT-ROOT type cr
   s" deep scratch sibling" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN ;

\ The text after needle in the child's stdout, to the end of its line; empty
\ when no line has it.
: GPT-LINE-AFTER$ ( n ptr u8 n -- ptr u8 n ) {: outu:n needle:ptr nu:n :}
   GPT-OUT outu needle nu FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   at 0 < if GPT-OUT 0 exit then
   GPT-OUT at nu + BYTE+ outu at nu + - {: rest:ptr restu:n :}
   rest restu STR-LF INDEX-OF MATCH option
      none OF restu ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: lineu:n :}
   rest lineu ;

\ text followed by E-FS-CAPACITY, the code REMOVE-TREE refuses the tree with.
: GPT-DS-CODE$ ( ptr u8 n -- ptr u8 n ) {: text:ptr textu:n :}
   SB-RESET
   text textu SB-APPEND
   E-FS-CAPACITY FMT:SB-INT
   SB$ ;

: GPT-DS-SCRATCH-LINE$ ( ptr u8 n -- ptr u8 n ) {: slot:ptr slotu:n :}
   SB-RESET
   s" test pool: scratch " SB-APPEND slot slotu SB-APPEND
   s"  of deep scratch row not removed, throw " SB-APPEND
   E-FS-CAPACITY FMT:SB-INT
   SB$ ;

: GPT-DS-EXPECT ( n ptr u8 n -- ) {: outu:n slot:ptr slotu:n :}
   s" deep scratch: the refusal names the row, its scratch and the code" T-LABEL
   GPT-OUT outu slot slotu GPT-DS-SCRATCH-LINE$ CONTAINS? TTRUE
   s" deep scratch: the row is red for it" T-LABEL
   GPT-OUT outu s" FAIL: deep scratch row" CONTAINS? TTRUE
   GPT-OUT outu s"  scratch=" GPT-DS-CODE$ CONTAINS? TTRUE
   GPT-OUT outu s" red tests: 1" CONTAINS? TTRUE
   s" deep scratch: the pool went on to the next row" T-LABEL
   GPT-OUT outu s" PASS: deep scratch sibling" CONTAINS? TTRUE
   s" deep scratch: the run's cleanup refusal is named" T-LABEL
   GPT-OUT outu s" test pool: cleanup threw " GPT-DS-CODE$ CONTAINS? TTRUE ;

\ The case's tree goes the way it came, a round at a time, then the child's
\ root with it.
: GPT-DS-CLEAN ( ptr u8 n ptr u8 n -- ) {: slot:ptr slotu:n root:ptr rootu:n :}
   slot slotu GPT-DS-UNDO
   root rootu REMOVE-TREE
   s" deep scratch: the case's tree is gone" T-LABEL
   root rootu EXISTS? TFALSE ;

: GPT-DS-REPORT ( -- )
   s" deep-scratch-case" GPT-MODE-CAPTURE {: outu:n erru:n code:n :}
   s" deep scratch: the run ends on the red report's exit" T-LABEL
   code 1 T=
   GPT-ERR erru s" test pool failed" CONTAINS? TTRUE
   s" deep scratch: the cleanup refusal is reported once" T-LABEL
   GPT-ERR erru s" cleanup at exit threw" CONTAINS? TFALSE
   outu s" deep slot: " GPT-LINE-AFTER$ {: slot:ptr slotu:n :}
   outu s" deep root: " GPT-LINE-AFTER$ {: root:ptr rootu:n :}
   s" deep scratch: the child named its slot and its root" T-LABEL
   slotu 0 > rootu 0 > and TTRUE
   slotu 0 > rootu 0 > and if
      outu slot slotu GPT-DS-EXPECT
      slot slotu root rootu GPT-DS-CLEAN
   then ;

\ A SUITE-LESS POOL USER REAPS ITS OWN ROOT (GT-POOL-FALLBACK-REMOVE).
\ A pool user with no GT-START - test/nf-path-test.f run standalone is the one
\ in the tree - captures into GT-POOL-FALLBACK-ROOT$, a directory the process
\ makes for itself and no runner cleanup knows about. Only its own drain can
\ remove it, so the pin is a spawned child that is such a user: it copies the
\ root before each drain and prints whether both directories went. The parent
\ is a suite, so its own root stays the runner's.
\
\ The child runs two pools because the root is memoised: the second round has
\ to build and capture into a root of its own, and a drain that removed the
\ directory without clearing the memo would red the child on E-FS-OPEN.
$400 constant GPT-FR-CAP
create GPT-FR-SRC GPT-FR-CAP allot
variable GPT-FR-U

: GPT-FR+ ( ptr u8 n -- )   GPT-FR-SRC GPT-FR-CAP GPT-FR-U BUF-APPEND ;

: GPT-FR-SRC$ ( -- ptr u8 n )
   GPT-FR-U BUF-RESET
   S\" require test/gate-pool.f\n" GPT-FR+
   S\" create FR-ROOT FS-PATH-CAP allot\nvariable FR-ROOT-U\n" GPT-FR+
   S\" : FR-SAVE ( ptr u8 n -- ) {: a:ptr u:n :}\n" GPT-FR+
   S\"    a FR-ROOT u BYTE-COPY u FR-ROOT-U ! ;\n" GPT-FR+
   S\" : FR-ROUND ( -- )\n" GPT-FR+
   S\"    GT-POOL-RESET\n" GPT-FR+
   S\"    s\" /usr/bin/true\" s\" fallback probe\" 4000 GT-POOL-START\n" GPT-FR+
   S\"    GT-POOL-FALLBACK-ROOT$ FR-SAVE\n" GPT-FR+
   S\"    GT-POOL-DRAIN ;\n" GPT-FR+
   S\" : FR-GONE? ( -- bool )\n" GPT-FR+
   S\"    FR-ROOT FR-ROOT-U @ EXISTS? 0= ;\n" GPT-FR+
   S\" : FR-MAIN ( -- )\n" GPT-FR+
   S\"    PROC-ENV-RESET PROC-ENV-INHERIT-MISSING\n" GPT-FR+
   S\"    FR-ROUND FR-GONE?\n" GPT-FR+
   S\"    FR-ROUND FR-GONE? and if\n" GPT-FR+
   S\"       s\" fallback root: gone\" type cr\n" GPT-FR+
   S\"    else\n" GPT-FR+
   S\"       s\" fallback root: kept\" type cr\n" GPT-FR+
   S\"    then ;\n" GPT-FR+
   S\" FR-MAIN\n" GPT-FR+
   GPT-FR-SRC GPT-FR-U @ ;

: GPT-FR-CASE ( -- )
   s" gate-pool-fallback-root" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   GPT-HB$ s" fallback root worker" GPT-FR-SRC$ GPT-TIMEOUT-MS GT-POOL-START-STDIN
   GT-POOL-DRAIN-SOFT
   s" fallback root: the child captured under two pools and exited clean" T-LABEL
   GT-POOL-RED# 0 T=
   s" fallback root: each of the child's drains removed the root it made" T-LABEL
   0 >IDX GT-POOL-OUT-BUF 0 >IDX GT-POOL-OUT-U-PTR @ s" fallback root: gone" CONTAINS? TTRUE
   GT-CLEANUP ;

\ The reader takes the engine's report and nothing shaped merely like it. Its
\ input is a captured stderr tail, so every fixture here is one.
: GPT-UNCAUGHT-CASE ( -- )
   s" uncaught: the engine's own report classifies" T-LABEL
   S\" hb: uncaught throw code -2502\n" GT-POOL-UNCAUGHT-TIMEOUT? TTRUE
   s" uncaught: a report behind the suite's output still classifies" T-LABEL
   S\" PASS: something\nhb: uncaught throw code -2502\n" GT-POOL-UNCAUGHT-TIMEOUT? TTRUE
   s" uncaught: another throw code does not" T-LABEL
   S\" hb: uncaught throw code -60\n" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: a longer code ending in those digits does not" T-LABEL
   S\" hb: uncaught throw code -12502\n" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: the code without its sign does not" T-LABEL
   S\" hb: uncaught throw code 2502\n" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: output after the report does not" T-LABEL
   S\" hb: uncaught throw code -2502\nand then more\n" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: a report with no digits does not" T-LABEL
   S\" hb: uncaught throw code -\n" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: a report with no newline does not" T-LABEL
   s" hb: uncaught throw code -2502" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: more digits than a code can have does not" T-LABEL
   S\" hb: uncaught throw code -11111111111111111111\n" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: the code on its own does not" T-LABEL
   S\" -2502\n" GT-POOL-UNCAUGHT-TIMEOUT? TFALSE
   s" uncaught: an empty capture does not" T-LABEL
   GPT-OUT 0 GT-POOL-UNCAUGHT-TIMEOUT? TFALSE ;

: GPT-INNER-TIMEOUT-RUN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   GPT-HB$ s" inner deadline worker" src srcu GPT-TIMEOUT-MS GT-POOL-START-STDIN
   GT-POOL-DRAIN-SOFT ;

\ Slot state, which is what the report is rendered from: the saturation depth
\ has to be snapshotted for this row the same way a pool-side kill snapshots
\ it, and an uncaught code that is not E-PROC-TIMEOUT has to stay an exit.
: GPT-INNER-TIMEOUT-CASE ( -- )
   s" gate-pool-inner-timeout" GT-START
   GPT-INNER-TIMEOUT-SRC$ GPT-INNER-TIMEOUT-RUN
   s" inner timeout: the child's own expired deadline is no plain exit" T-LABEL
   0 >IDX GT-POOL-EXITED-PTR @ TFALSE
   s" inner timeout: the slot reports a load timeout" T-LABEL
   0 >IDX GT-POOL-TIMED-OUT-PTR @ TTRUE
   s" inner timeout: the row is red" T-LABEL
   GT-POOL-RED# 1 T=
   s" inner timeout: the red row is a timeout row" T-LABEL
   0 GT-POOL-RED-TIMED-OUT-PTR @ TTRUE
   0 GT-POOL-RED-EXITED-PTR @ TFALSE
   s" inner timeout: the red row carries the saturation depth" T-LABEL
   0 GT-POOL-RED-SAT-LIVE-PTR @ 1 T=
   0 GT-POOL-RED-SAT-LIMIT-PTR @ 1 T=
   GPT-OTHER-THROW-SRC$ GPT-INNER-TIMEOUT-RUN
   s" inner timeout: another uncaught code stays an exit" T-LABEL
   0 >IDX GT-POOL-EXITED-PTR @ TTRUE
   0 >IDX GT-POOL-TIMED-OUT-PTR @ TFALSE
   0 >IDX GT-POOL-CODE-PTR @ UNCAUGHT-RC T=
   GT-CLEANUP ;

\ A ROW BOUNDED BY ITS OWN WORK (GT-POOL-CPU-BUDGET!). A budgeted row is ended
\ once its process tree has run its CPU budget, and its wall deadline is only a
\ hang guard. The ways this can fail, written down before the pool counted CPU
\ time:
\  1. WALL TIME COUNTED AS WORK. A row that only waits - a build starved of a
\     core on a saturated host - is ended at its budget.
\  2. WORK UNCOUNTED. A row that spins outlives its budget and meets only its
\     wall deadline: the reading is in the wrong unit (libproc counts mach time
\     units, not nanoseconds) or misses the process at work.
\  3. DESCENDANTS UNCOUNTED. A build row's work is its grandchild's, the
\     builder's, and the row's own process never reaches the budget.
\  4. REAPED WORK UNCOUNTED. A child's time passes to its parent when the
\     parent waits for it, and a reading of the live processes drops it.
\  5. THE HANG GUARD DROPPED. A row that stopped running gains no CPU time, so
\     only its wall deadline can end it.
\  6. A BUDGET KILL REPORTED AS LOAD (GPT-BATTERY-TIMEOUT): the row ran past
\     its budget at any load.
\ The red record is what the report is rendered from, so each case reads it.
500 constant GPT-CPU-MS                  \ the budget of a row that is ended for it
\ That row's hang guard: far past its budget, and short of the 20.8 s of CPU a
\ reading in raw mach units (125/3 ns each here) would need to pass it.
15000 constant GPT-CPU-GUARD-MS
1500 constant GPT-CPU-WAIT-MS            \ wall time spent waiting, past the budget
600 constant GPT-CPU-CHILD-MS            \ case 4: the grandchild's CPU time
700 constant GPT-CPU-SELF-MS             \ and the worker's own: each under the budget
1000 constant GPT-CPU-SUM-MS             \ case 4's budget, which both together pass
5000 constant GPT-CPU-HUNG-MS            \ case 5: a budget the stopped row never reaches
1000 constant GPT-CPU-HANG-MS            \ case 5's hang guard

\ Run ms of this thread's own CPU time.
: GPT-SPIN-MS ( n -- ) {: ms:n :}
   TIME:THREAD-CPU-NS ms PROC-NS-PER-MS * + {: done:n :}
   begin TIME:THREAD-CPU-NS done >= until ;

: GPT-CPU-WAITER ( -- )
   GPT-CPU-WAIT-MS GPT-SLEEP-MS ;

\ The work is the grandchild's; the worker only waits.
: GPT-CPU-GRAND-SPIN ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if GPT-SPIN-WORKER then
   GPT-CPU-GUARD-MS GPT-SLEEP-MS ;

: GPT-CPU-GRANDCHILD ( -- )
   GPT-CPU-CHILD-MS GPT-SPIN-MS
   s" " 0 die ;

\ The worker reaps its grandchild, runs its own share and waits out a reading.
: GPT-CPU-REAPS ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if GPT-CPU-GRANDCHILD then
   pid PROC-WAIT-STATUS drop
   GPT-CPU-SELF-MS GPT-SPIN-MS
   GPT-CPU-WAIT-MS GPT-SLEEP-MS ;

: GPT-CPU-STOPPED ( -- )
   GPT-CPU-GUARD-MS GPT-SLEEP-MS ;

: GPT-CPU-RUN ( ptr u8 n n n [ -- ] -- ) {: label:ptr labelu:n budget:n guard:n q :}
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   label labelu guard q GT-POOL-START-FORK
   budget GT-POOL-SEQ @ GT-POOL-CPU-BUDGET!
   GT-POOL-DRAIN-SOFT ;

: GPT-CPU-ENDED ( -- )
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-TIMED-OUT-PTR @ TTRUE
   0 GT-POOL-RED-OVER? TTRUE ;

: GPT-CPU-BUDGET-CASE ( -- )
   s" gate-pool-cpu-budget" GT-START
   s" cpu waiter" GPT-CPU-MS GPT-CPU-GUARD-MS [: GPT-CPU-WAITER ;] GPT-CPU-RUN
   s" cpu budget: a row that waits past its budget is not ended for it" T-LABEL
   GT-POOL-RED# 0 T=
   s" cpu spinner" GPT-CPU-MS GPT-CPU-GUARD-MS [: GPT-SPIN-WORKER ;] GPT-CPU-RUN
   s" cpu budget: a row that spins past its budget is ended for it" T-LABEL
   GPT-CPU-ENDED
   s" cpu grandchild" GPT-CPU-MS GPT-CPU-GUARD-MS [: GPT-CPU-GRAND-SPIN ;] GPT-CPU-RUN
   s" cpu budget: a grandchild's work counts" T-LABEL
   GPT-CPU-ENDED
   s" cpu reaped" GPT-CPU-SUM-MS GPT-CPU-GUARD-MS [: GPT-CPU-REAPS ;] GPT-CPU-RUN
   s" cpu budget: a reaped grandchild's work counts" T-LABEL
   GPT-CPU-ENDED
   s" cpu stopped" GPT-CPU-HUNG-MS GPT-CPU-HANG-MS [: GPT-CPU-STOPPED ;] GPT-CPU-RUN
   s" cpu budget: a row that stopped running meets its hang guard" T-LABEL
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-TIMED-OUT-PTR @ TTRUE
   0 GT-POOL-RED-OVER? TFALSE
   0 GT-POOL-RED-CPU-BUDGET-PTR @ GPT-CPU-HUNG-MS T=
   GT-CLEANUP ;

: GATE-POOL-TEST-MAIN ( -- )
   s" fail-battery-case" GPT-MODE? if GPT-BATTERY-CASE exit then
   s" deep-scratch-case" GPT-MODE? if GPT-DS-CASE exit then
   T-RESET
   0 GPT-COW !
   s" gate-pool-test" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork worker" 1000 [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GPT-COW @ 0 T=
   GPT-BIG-CASE
   GPT-INFRA-CASE
   GT-CLEANUP
   GPT-GROUP-KILL-CASE
   GPT-WAIT-NEG-CASE
   GPT-EXTERNAL-KILL-CASE
   GPT-FC-CASE
   GPT-STDIN-CASE
   GPT-REAPER-REFUSED-CASE
   GPT-REAPER-FORK-CASE
   GPT-CHILD-TMP-CASE
   GPT-DS-REPORT
   GPT-FR-CASE
   GPT-UNCAUGHT-CASE
   GPT-INNER-TIMEOUT-CASE
   GPT-CPU-BUDGET-CASE
   GPT-BATTERY-REPORT
   T-REPORT
   s" gate-pool-test: ok" type cr ;

\ The battery runs at load time, so it is dispatched from top-level scope:
\ a package is still open above this line.
' GATE-POOL-TEST-MAIN
;package
execute

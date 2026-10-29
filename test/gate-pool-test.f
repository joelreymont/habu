\ gate-pool-test.f - focused coverage for fork-backed test pool workers.
\ Run: bin/hb --load test/gate-pool-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/string-roles.f               \ package STR: the typed string surface
require src/lib/fmt.f
require src/lib/adt/option.f                 \ option<NUM:index> STR:INDEX-OF consumer (switchover wave A)
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-fork.f
require src/lib/test/runner.f
require src/lib/engine-candidate.f
require test/gate-pool.f
require test/pcap-result.f

\ The private FS words this test reads, forwarded into its own package.
package FS
: GATE-POOL-TEST:CLEANUP-COUNT ( -- n ) CLEANUP-N @ ;
: GATE-POOL-TEST:MODE-DIR ( -- n ) MODE-DIR ;
;package

package GATE-POOL-TEST
private

$20000 constant GPT-CAP
$2710 constant GPT-TIMEOUT-MS
$100 constant GPT-BIG-CHUNK
400 constant GPT-BIG-N
GPT-BIG-CHUNK GPT-BIG-N * constant GPT-BIG-BYTES
160 constant GPT-SPEW-N
12 constant GPT-OVERFLOW-N

create GPT-OUT GPT-CAP allot
create GPT-ERR GPT-CAP allot
create GPT-BIG-BUF GPT-BIG-CHUNK allot
create GPT-ROOT-SAVE PATH-CAP allot

variable GPT-COW
TYPED-VARIABLE GPT-BIG-A ptr u8
variable GPT-COUNT-N
variable GPT-ROOT-SAVE-U

\ The engine under test: the gate's freshly built candidate when it exported
\ one, else the running engine itself (src/lib/engine-candidate.f).
: GPT-HB$ ( -- ptr u8 n )    ENGINE-CANDIDATE:PATH$ ;

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

: GPT-BATTERY-TRUNC ( -- )
   s" fork failing worker" [: GPT-FAIL-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-SOFT ( -- )
   s" soft fail one" [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   s" soft fail two" [: GPT-SOFT-TWO ;] GT-POOL-START-FORK
   s" soft pass" [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-WRAP ( -- )
   s" wrap throw worker" [: GPT-WRAP-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-OVERFLOW ( -- )
   GPT-OVERFLOW-N 0 ?do
      s" soft overflow" [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   loop ;

\ One child process runs every failure class; reds accumulate to 16 and the
\ final fail-closed drain lists every one of them.
: GPT-BATTERY-CASE ( -- )
   s" gate-pool-test-battery" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   GPT-BATTERY-TRUNC
   GPT-BATTERY-SOFT
   GPT-BATTERY-WRAP
   GPT-BATTERY-OVERFLOW
   GT-POOL-DRAIN ;

: GPT-MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   0 0= 0= ;

\ The child inherits this process's environment, so the root its GT-START
\ makes lies under the TMPDIR this run was given.
: GPT-MODE-CAPTURE-MS ( ptr u8 n n -- n n n ) {: mode:ptr modeu:n ms:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-pool-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   GPT-HB$ >LEN GPT-OUT GPT-CAP >LEN GPT-ERR GPT-CAP >LEN
   ms >MS RUN-ARGV-ENV-CAPTURE
   PCAP-RESULT:>N ;

: GPT-MODE-CAPTURE ( ptr u8 n -- n n n )
   GPT-TIMEOUT-MS GPT-MODE-CAPTURE-MS ;

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

: GPT-EXPECT-OVERFLOW-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" red tests: 16" CONTAINS? TTRUE
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
   outu GPT-EXPECT-OVERFLOW-OUT
   erru GPT-EXPECT-BATTERY-ERR ;

: GPT-BIG-ALLOC ( -- )
   GPT-BIG-A @ 0= if
      GPT-BIG-BYTES $400 + GT-POOL-ALLOC-BYTES GPT-BIG-A !
   then ;

: GPT-BIG-FILE-LEN ( -- n )
   0 >IDX GT-POOL-OUT-FILE$ GPT-BIG-A @ GPT-BIG-BYTES $400 + FS:READ-ALL ;

: GPT-BIG-EXPECT ( -- )
   0 >IDX GT-POOL-OUT-U-PTR @ GT-OUT-CAP T=
   0 >IDX GT-POOL-OUT-TOTAL-PTR @ GPT-BIG-BYTES T=
   0 >IDX GT-POOL-OUT-FILE$ FS:FILE? TTRUE
   GPT-BIG-FILE-LEN GPT-BIG-BYTES T=
   GPT-BIG-A @ c@ $41 T=
   GPT-BIG-A @ GPT-BIG-BYTES 1- BYTE+ c@ $41 T=
   0 >IDX GT-POOL-OUT-BUF c@ $41 T= ;

: GPT-BIG-CASE ( -- )
   GPT-BIG-ALLOC
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork big output" [: GPT-BIG-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GPT-BIG-EXPECT ;

: GPT-ROOT-SAVE! ( -- )
   GT-ROOT {: a:ptr u:n :}
   u PATH-CAP > if E-FS-PATH throw then
   a GPT-ROOT-SAVE u BYTE-COPY
   u GPT-ROOT-SAVE-U ! ;

: GPT-ROOT-RESTORE ( -- )
   GPT-ROOT-SAVE GPT-ROOT-SAVE-U @ GT-COPY-ROOT! ;

: GPT-INFRA-START ( -- )
   s" infra fork" [: GPT-WORKER ;] GT-POOL-START-FORK ;

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
\ sentinel after a delay, then exits. The grandchild still holds the row's
\ output pipes, so at the worker's exit the pool kills its tree, the zombie
\ leader's group with it, and the grandchild dies before its delay elapses
\ and the sentinel never appears.
\ The case waits for that death itself, not for a fixed settle: the worker, its
\ reaper and the grandchild all inherit the write end of a pipe the case opens
\ before the fork and closes in itself, so the read end reports EOF once the
\ last of them is gone. A grandchild the kill missed holds it open until it has
\ written the sentinel, DELAY after it started; one that never goes fails the
\ GONE deadline.
1500 constant GPT-GK-DELAY-MS
3000 constant GPT-GK-GONE-MS       \ past DELAY, so a grandchild the kill missed has written by then

create GPT-GK-SENTINEL PATH-CAP allot
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
   GPT-GK-SENTINEL$ s" grandchild-was-here" FS:WRITE-ALL
   s" " 0 die ;

: GPT-GK-CHILD ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if GPT-GK-GRANDCHILD then
   s" gate-pool group-kill child" type cr ;

\ True once every holder of the pipe's write end has exited.
: GPT-GK-GONE? ( fd -- bool ) {: rd:fd :}
   rd POLLIN PROC-PFD!
   1 GPT-GK-GONE-MS GPT-GK-GONE-MS >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

: GPT-GK-SENTINEL! ( -- )
   GT-ROOT s" group-kill-sentinel" GPT-GK-SENTINEL FS:JOIN-PATH GPT-GK-SENTINEL-U ! ;

: GPT-GROUP-KILL-CASE ( -- )
   s" gate-pool-group-kill" GT-START
   GPT-GK-SENTINEL!
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   PIPE-PAIR {: rd:fd wr:fd :}
   s" group-kill worker" [: GPT-GK-CHILD ;] GT-POOL-START-FORK
   wr FD>N close
   GT-POOL-DRAIN-SOFT
   s" group-kill: the worker's own exit is its outcome" T-LABEL
   GT-POOL-RED# 0 T=
   s" group-kill: the worker and everything it started are gone" T-LABEL
   rd GPT-GK-GONE? TTRUE
   rd FD>N close
   s" group-kill: grandchild sentinel absent" T-LABEL
   GPT-GK-SENTINEL$ FS:EXISTS? TFALSE
   GT-CLEANUP ;

\ A FALLING CPU READING PRINTS A NEGATIVE DELTA. A row's tree loses CPU time
\ when a member leaves it: the worker forks A, A forks B, B spins, then A exits
\ between the row's first WAIT line and its second, and B, reparented to init,
\ leaves the tree with its time; B ends before the second WAIT line, so nothing
\ outlives the case. The second WAIT line prints the fall signed and the pool
\ goes on. The row outlives two heartbeats, and the WAIT lines and an uncaught
\ throw belong to the gate process, so it runs in a child of its own.
500 constant GPT-CF-SPIN-MS
GT-HEARTBEAT-MS 3 * 2 / constant GPT-CF-LEAVE-MS    \ between the WAIT lines
GPT-CF-LEAVE-MS 1000 + constant GPT-CF-B-MS         \ B's sleep, past A's exit
GT-HEARTBEAT-MS 5 * 2 / constant GPT-CF-END-MS       \ between the second WAIT line and a third
GPT-CF-END-MS GPT-TIMEOUT-MS + constant GPT-CF-TIMEOUT-MS

: GPT-CF-SPIN ( -- )
   mono-ns GPT-CF-SPIN-MS PROC-NS-PER-MS * + {: deadline:n :}
   begin mono-ns deadline < while repeat ;

: GPT-CF-B ( -- )
   GPT-CF-SPIN
   GPT-CF-B-MS GPT-SLEEP-MS
   s" " 0 die ;

: GPT-CF-A ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if GPT-CF-B then
   GPT-CF-LEAVE-MS GPT-SLEEP-MS
   s" " 0 die ;

: GPT-CF-WORKER ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if GPT-CF-A then
   GPT-CF-END-MS GPT-SLEEP-MS ;

: GPT-CF-CASE ( -- )
   s" gate-pool-cpu-fall" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" cpu fall worker" [: GPT-CF-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   s" cpu fall: drained" type cr
   GT-CLEANUP ;

: GPT-CF-REPORT ( -- )
   s" cpu-fall-case" GPT-CF-TIMEOUT-MS GPT-MODE-CAPTURE-MS nip
   {: outu:n code:n :}
   s" cpu fall: the second WAIT line prints the fall signed" T-LABEL
   GPT-OUT outu s"  ms, -" CONTAINS? TTRUE
   s" cpu fall: the pool drained the row and the child exited clean" T-LABEL
   GPT-OUT outu s" cpu fall: drained" CONTAINS? TTRUE
   code 0 T= ;

\ Regression for the parent-death reaper's wait(-1) conflict: the reaper armed
\ by GT-POOL-FORK-CHILD must be reparented away from the worker (double-fork),
\ so a worker body that calls wait(-1) expecting no children of its own still
\ gets ECHILD instead of blocking forever on the reaper. This is exactly the
\ case that stalled stdlib/tail-process (src/lib/process-test.f TEST-WAIT-BAD) when
\ the reaper was a direct child. If the reaper regresses to a worker child, the
\ worker blocks and its WAIT lines show a CPU delta of zero.
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
   s" wait-neg worker" [: GPT-WAIT-NEG-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   GT-CLEANUP ;

\ A child that dies by a SIGKILL the pool did NOT send: it signals its own
\ process group (an OOM/AMFI kill is the real-world analog). It reaps as
\ signaled(9) - rc 137 when flattened - and is a plain FAIL (kind=signal).
\ waitpid's WIFSIGNALED vs WIFEXITED (PROC-STATUS>OUTCOME) keeps a
\ self-exit(137) distinct: that would read exited(137)/kind=exit.
: GPT-EXTERNAL-KILL-WORKER ( -- )
   s" gate-pool external-kill worker" type cr
   0 >PID SIGKILL PROC-KILL-RAW drop ;

: GPT-EXTERNAL-KILL-CASE ( -- )
   s" gate-pool-external-kill" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" external-kill worker" [: GPT-EXTERNAL-KILL-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   s" external-kill: pool-unsent SIGKILL is a plain signal FAIL" T-LABEL
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-EXITED-PTR @ TFALSE
   0 GT-POOL-RED-CODE-PTR @ SIGKILL T=
   GT-CLEANUP ;

\ Fork-inherited cleanup registrations. src/lib/fs-mutate.f's cleanup table records
\ the paths THIS process owns and FS:CLEANUP-RUN deletes every entry in it, so a
\ fork child that inherits the parent's entries and runs its own cleanups
\ deletes paths the live parent still owns. In the gate that was the driver's
\ whole capture root, removed out from under its running siblings (E-FS-OPEN /
\ E-FS-IO on a rotating victim). PROC-FORK:RAW empties the table in the child,
\ so a child cleans up exactly what the child registered.
\
\ The evidence lives in a private temp tree that is deliberately NOT registered
\ for cleanup: an unfixed child also runs the inherited GT-ROOT entry, and
\ evidence kept under GT-ROOT would be erased by the very bug it records.
create GPT-FC-EV PATH-CAP allot        \ evidence root; never registered
create GPT-FC-PARENT PATH-CAP allot    \ registered by the parent, before the fork
create GPT-FC-PARENT-FILE PATH-CAP allot
create GPT-FC-CHILD PATH-CAP allot     \ registered by the child, after the fork
create GPT-FC-CHILD-FILE PATH-CAP allot
create GPT-FC-DEPTH PATH-CAP allot     \ one byte: 1 when the child inherited an entry
create GPT-FC-DONE PATH-CAP allot      \ written after the child's FS:CLEANUP-RUN returns
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
   u PATH-CAP > if E-FS-PATH throw then
   a GPT-FC-EV u BYTE-COPY
   u GPT-FC-EV-U ! ;

: GPT-FC-SUB! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: base:ptr baseu:n name:ptr nameu:n dst:ptr up:ptr :}
   base baseu name nameu dst FS:JOIN-PATH up ! ;

: GPT-FC-PATHS! ( -- )
   s" hb-fork-cleanup" FS:MKTEMP-DIR GPT-FC-EV!
   GPT-FC-EV$ s" parent" GPT-FC-PARENT GPT-FC-PARENT-U GPT-FC-SUB!
   GPT-FC-PARENT$ s" keep" GPT-FC-PARENT-FILE GPT-FC-PARENT-FILE-U GPT-FC-SUB!
   GPT-FC-EV$ s" child" GPT-FC-CHILD GPT-FC-CHILD-U GPT-FC-SUB!
   GPT-FC-CHILD$ s" mark" GPT-FC-CHILD-FILE GPT-FC-CHILD-FILE-U GPT-FC-SUB!
   GPT-FC-EV$ s" depth" GPT-FC-DEPTH GPT-FC-DEPTH-U GPT-FC-SUB!
   GPT-FC-EV$ s" done" GPT-FC-DONE GPT-FC-DONE-U GPT-FC-SUB! ;

: GPT-FC-REGISTER-PARENT ( -- )
   GPT-FC-PARENT$ FS:MAKE-DIRS
   GPT-FC-PARENT-FILE$ s" keep" FS:WRITE-ALL
   GPT-FC-PARENT$ FS:CLEANUP-TREE+ ;

\ The fork worker records whether it inherited any cleanup entry - one byte, 1
\ or 0, written before it touches the table - then registers and runs its OWN
\ cleanup. It asserts nothing: a failed assertion in a fork child still exits 0, so every
\ verdict belongs to the parent.
: GPT-FC-WORKER ( -- )
   CLEANUP-COUNT 0<> if 1 else 0 then GPT-FC-BYTE c!
   GPT-FC-DEPTH$ GPT-FC-BYTE 1 FS:WRITE-ALL
   GPT-FC-CHILD$ FS:MAKE-DIRS
   GPT-FC-CHILD-FILE$ s" mark" FS:WRITE-ALL
   GPT-FC-CHILD$ FS:CLEANUP-TREE+
   FS:CLEANUP-RUN
   GPT-FC-DONE$ s" done" FS:WRITE-ALL ;

\ Poison the byte before reading it, so a short or empty read cannot pass for a
\ recorded 0, and demand exactly the one byte the child wrote.
: GPT-FC-DEPTH@ ( -- n )
   $FF GPT-FC-BYTE c!
   GPT-FC-DEPTH$ GPT-FC-BYTE 1 FS:READ-ALL {: got:n :}
   got 1 <> if E-FS-IO throw then
   GPT-FC-BYTE c@ ;

: GPT-FC-CASE ( -- )
   s" gate-pool-fork-cleanup" GT-START     \ a registered capture root: the driver's own shape
   GPT-FC-PATHS!
   GPT-FC-REGISTER-PARENT
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" fork cleanup worker" [: GPT-FC-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   s" fork cleanup: the child inherits an empty cleanup table" T-LABEL
   GPT-FC-DEPTH@ 0 T=
   s" fork cleanup: the child ran its own cleanup" T-LABEL
   GPT-FC-CHILD$ FS:EXISTS? TFALSE
   s" fork cleanup: the child ran to completion" T-LABEL
   GPT-FC-DONE$ FS:FILE? TTRUE
   s" fork cleanup: the parent's directory survives the child" T-LABEL
   GPT-FC-PARENT$ FS:DIR? TTRUE
   s" fork cleanup: the parent's file survives the child" T-LABEL
   GPT-FC-PARENT-FILE$ FS:FILE? TTRUE
   GT-CLEANUP
   s" fork cleanup: the parent's own run removes what it registered" T-LABEL
   GPT-FC-PARENT$ FS:EXISTS? TFALSE
   GPT-FC-EV$ FS:REMOVE-TREE ;

\ A stdin-fed slot: the child engine reads its source from the pool's pipe,
\ and its output reaches the slot's capture like any spawned child's.
: GPT-STDIN-SRC$ ( -- ptr u8 n )
   S\" s\" gate-pool stdin worker\" type cr\n" ;

: GPT-STDIN-CASE ( -- )
   s" gate-pool-stdin" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   GPT-HB$ s" stdin worker" GPT-STDIN-SRC$ GT-POOL-START-STDIN
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
   s" /bin/sleep" s" reaper refused" GT-POOL-START ;

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
   s" reaper-fork sibling" [: GPT-RF-SIBLING ;] GT-POOL-START-FORK
   0 >IDX GT-POOL-PID@ PID>N GPT-RF-SIB-PID !
   0 GPT-RF-GEN !
   [: GPT-RF-FORK ;] is PROC-FORK:FORK-CALL
   s" reaper-fork refused" [: GPT-RF-VICTIM ;] GT-POOL-START-FORK
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
\ Each spawned child is given a directory of its own as TMPDIR, so the tree it
\ makes the ordinary way - src/lib/fs-mutate.f FS:MKTEMP-DIR - lands inside it,
\ and the pool removes that directory when the slot retires. The killed child
\ is the case nothing else covers: it never reaches a cleanup of its own, so
\ before this the tree it made outlived every run.
\
\ Each child prints the path it made; the slot's capture is how the parent
\ learns it. The killed child ends itself with SIGKILL to its own group.

: GPT-CT-GREEN-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\n: W ( -- ) s\" gpt-child\" FS:MKTEMP-DIR type cr ;\nW\n" ;

: GPT-CT-KILL-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fs-mutate.f\nrequire src/lib/process.f\n: W ( -- ) s\" gpt-child\" FS:MKTEMP-DIR type cr 0 >PID SIGKILL PROC-KILL-RAW drop ;\nW\n" ;

: GPT-CT-PRINTED$ ( -- ptr u8 n )        \ slot 0's captured line, without its newline
   0 >IDX GT-POOL-OUT-BUF 0 >IDX GT-POOL-OUT-U-PTR @ {: a:ptr u:n :}
   u 0 > if
      a u 1- BYTE+ c@ STR-LF = if a u 1- exit then
   then
   a u ;

\ The environment is inherited first, TMPDIR and all: the pool's own row has to
\ REPLACE an inherited one, not queue up behind it, or the child resolves the
\ caller's TMPDIR and makes its tree outside the slot's directory. A row the
\ caller set to a value of its own is the opposite case and stays; that one is
\ checked where it is needed, test/nf-path-test.f (a 97-byte root reaches the
\ build unchanged).
: GPT-CT-RUN ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n label:ptr labelu:n :}
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   GPT-HB$ label labelu src srcu GT-POOL-START-STDIN
   GT-POOL-DRAIN-SOFT ;

: GPT-CT-EXPECT ( -- )
   s" child tmp: the slot was given a scratch directory" T-LABEL
   0 >IDX GT-POOL-TMP-PATH-U-PTR @ 0 > TTRUE
   s" child tmp: the child made its tree under that directory" T-LABEL
   GPT-CT-PRINTED$ 0 >IDX GT-POOL-TMP$ STARTS-WITH? TTRUE
   s" child tmp: the child's tree is gone after the drain" T-LABEL
   GPT-CT-PRINTED$ FS:EXISTS? TFALSE
   s" child tmp: the slot's scratch directory is gone too" T-LABEL
   0 >IDX GT-POOL-TMP$ FS:EXISTS? TFALSE ;

: GPT-CHILD-TMP-CASE ( -- )
   s" gate-pool-child-tmp" GT-START
   GPT-CT-KILL-SRC$ s" child tmp kill worker" GPT-CT-RUN
   s" child tmp: the killed child died of SIGKILL" T-LABEL
   0 >IDX GT-POOL-EXITED-PTR @ TFALSE
   0 >IDX GT-POOL-CODE-PTR @ SIGKILL T=
   GPT-CT-EXPECT
   GPT-CT-GREEN-SRC$ s" child tmp green worker" GPT-CT-RUN
   s" child tmp: the green child exited clean" T-LABEL
   GT-POOL-RED# 0 T=
   GPT-CT-EXPECT
   GT-CLEANUP ;

\ A SCRATCH THAT WILL NOT GO FAILS ITS ROW, NOT THE POOL (gate-pool.f
\ GT-POOL-CHILD-TMP-REMOVE). A row can leave a tree under its TMPDIR that
\ FS:REMOVE-TREE cannot take apart: a directory the row locked, holding an entry,
\ refuses that entry's unlink with EACCES and the removal throws E-FS-IO. The
\ refusal is named with the row, the path and the code, the row is red, and the
\ pool goes on to the next row; the run's own cleanup meets the same tree under
\ the root and is named too, before the red report's exit. The case runs in a
\ child (mode locked-scratch-case), so that exit is the child's.
\
\ The tree is slot/d/d, the outer d locked (mode 0555) over the inner one.
$16D constant GPT-DS-LOCKED-MODE

create GPT-DS-PATH PATH-CAP allot

\ slot/d/d/... with level components below the slot, in GPT-DS-PATH.
: GPT-DS-PATH$ ( ptr u8 n n -- ptr u8 n ) {: slot:ptr slotu:n level:n :}
   slotu 0 ?do slot i + c@ GPT-DS-PATH i + c! loop
   level 0 ?do
      FS:SLASH GPT-DS-PATH slotu i 2 * + + c!
      100 GPT-DS-PATH slotu i 2 * + 1+ + c!
   loop
   GPT-DS-PATH slotu level 2 * + ;

: GPT-DS-MAKE ( ptr u8 n -- ) {: slot:ptr slotu:n :}
   slot slotu 2 GPT-DS-PATH$ FS:MAKE-DIRS
   slot slotu 1 GPT-DS-PATH$ GPT-DS-LOCKED-MODE FS:CHMOD-MODE ;

: GPT-DS-CASE ( -- )
   s" gate-pool-locked-scratch" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/true" s" locked scratch row" GT-POOL-START
   0 >IDX GT-POOL-TMP$ GPT-DS-MAKE
   s" locked slot: " type 0 >IDX GT-POOL-TMP$ type cr
   s" locked root: " type GT-ROOT type cr
   s" locked scratch sibling" [: GPT-WORKER ;] GT-POOL-START-FORK
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

\ text followed by E-FS-IO, the code FS:REMOVE-TREE refuses the tree with.
: GPT-DS-CODE$ ( ptr u8 n -- ptr u8 n ) {: text:ptr textu:n :}
   SB-RESET
   text textu SB-APPEND
   E-FS-IO FMT:SB-INT
   SB$ ;

: GPT-DS-SCRATCH-LINE$ ( ptr u8 n -- ptr u8 n ) {: slot:ptr slotu:n :}
   SB-RESET
   s" test pool: scratch " SB-APPEND slot slotu SB-APPEND
   s"  of locked scratch row not removed, throw " SB-APPEND
   E-FS-IO FMT:SB-INT
   SB$ ;

: GPT-DS-EXPECT ( n ptr u8 n -- ) {: outu:n slot:ptr slotu:n :}
   s" locked scratch: the refusal names the row, its scratch and the code" T-LABEL
   GPT-OUT outu slot slotu GPT-DS-SCRATCH-LINE$ CONTAINS? TTRUE
   s" locked scratch: the row is red for it" T-LABEL
   GPT-OUT outu s" FAIL: locked scratch row" CONTAINS? TTRUE
   GPT-OUT outu s"  scratch=" GPT-DS-CODE$ CONTAINS? TTRUE
   GPT-OUT outu s" red tests: 1" CONTAINS? TTRUE
   s" locked scratch: the pool went on to the next row" T-LABEL
   GPT-OUT outu s" PASS: locked scratch sibling" CONTAINS? TTRUE
   s" locked scratch: the run's cleanup refusal is named" T-LABEL
   GPT-OUT outu s" test pool: cleanup threw " GPT-DS-CODE$ CONTAINS? TTRUE ;

\ The case's tree goes once its lock is lifted, with the child's root.
: GPT-DS-CLEAN ( ptr u8 n ptr u8 n -- ) {: slot:ptr slotu:n root:ptr rootu:n :}
   slot slotu 1 GPT-DS-PATH$ MODE-DIR FS:CHMOD-MODE
   root rootu FS:REMOVE-TREE
   s" locked scratch: the case's tree is gone" T-LABEL
   root rootu FS:EXISTS? TFALSE ;

: GPT-DS-REPORT ( -- )
   s" locked-scratch-case" GPT-MODE-CAPTURE {: outu:n erru:n code:n :}
   s" locked scratch: the run ends on the red report's exit" T-LABEL
   code 1 T=
   GPT-ERR erru s" test pool failed" CONTAINS? TTRUE
   s" locked scratch: the cleanup refusal is reported once" T-LABEL
   GPT-ERR erru s" cleanup at exit threw" CONTAINS? TFALSE
   outu s" locked slot: " GPT-LINE-AFTER$ {: slot:ptr slotu:n :}
   outu s" locked root: " GPT-LINE-AFTER$ {: root:ptr rootu:n :}
   s" locked scratch: the child named its slot and its root" T-LABEL
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
   S\" create FR-ROOT PATH-CAP allot\nvariable FR-ROOT-U\n" GPT-FR+
   S\" : FR-SAVE ( ptr u8 n -- ) {: a:ptr u:n :}\n" GPT-FR+
   S\"    a FR-ROOT u BYTE-COPY u FR-ROOT-U ! ;\n" GPT-FR+
   S\" : FR-ROUND ( -- )\n" GPT-FR+
   S\"    GT-POOL-RESET\n" GPT-FR+
   S\"    s\" /usr/bin/true\" s\" fallback probe\" GT-POOL-START\n" GPT-FR+
   S\"    GT-POOL-FALLBACK-ROOT$ FR-SAVE\n" GPT-FR+
   S\"    GT-POOL-DRAIN ;\n" GPT-FR+
   S\" : FR-GONE? ( -- bool )\n" GPT-FR+
   S\"    FR-ROOT FR-ROOT-U @ FS:EXISTS? 0= ;\n" GPT-FR+
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
   GPT-HB$ s" fallback root worker" GPT-FR-SRC$ GT-POOL-START-STDIN
   GT-POOL-DRAIN-SOFT
   s" fallback root: the child captured under two pools and exited clean" T-LABEL
   GT-POOL-RED# 0 T=
   s" fallback root: each of the child's drains removed the root it made" T-LABEL
   0 >IDX GT-POOL-OUT-BUF 0 >IDX GT-POOL-OUT-U-PTR @ s" fallback root: gone" CONTAINS? TTRUE
   GT-CLEANUP ;

: GATE-POOL-TEST-MAIN ( -- )
   s" fail-battery-case" GPT-MODE? if GPT-BATTERY-CASE exit then
   s" locked-scratch-case" GPT-MODE? if GPT-DS-CASE exit then
   s" cpu-fall-case" GPT-MODE? if GPT-CF-CASE exit then
   T-RESET
   0 GPT-COW !
   s" gate-pool-test" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" fork worker" [: GPT-WORKER ;] GT-POOL-START-FORK
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
   GPT-CF-REPORT
   GPT-BATTERY-REPORT
   T-REPORT
   s" gate-pool-test: ok" type cr ;

\ The battery runs at load time, so it is dispatched from top-level scope:
\ a package is still open above this line.
' GATE-POOL-TEST-MAIN
;package
execute

\ gate-pool-test.f - focused coverage for fork-backed test pool workers.
\ Run: bin/hb --load test/gate-pool-test.f

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<CAD-NUM:index> STR:INDEX-OF consumer (switchover wave A)
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/test/runner.f
require test/gate-pool.f

\ White-box CAD-NUM role reader (precedent: lib/string-test.f STR-T-IX>RAW):
\ reopen the unsealed CAD-NUM package to project the typed STR:FIND-SUB /
\ STR:INDEX-OF index back to its raw cell. A plain checked word over the audited
\ private INDEX>N projection - not a new boundary.
package CAD-NUM
public
: GPT-IX>RAW ( CAD-NUM:index -- n ) INDEX>N ;
;package

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
variable GPT-BIG-A
variable GPT-COUNT-N
variable GPT-ROOT-SAVE-U

: GPT-BIG-A-FIELD ( -- ptr ptr u8 )
   GPT-BIG-A 0 ptr-field ;

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

: GPT-BATTERY-TIMEOUT ( -- )
   s" hang worker" GPT-HANG-TIMEOUT-MS [: GPT-HANG-WORKER ;] GT-POOL-START-FORK
   s" quick worker" GPT-TIMEOUT-MS [: GPT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT ;

: GPT-BATTERY-OVERFLOW ( -- )
   GPT-OVERFLOW-N 0 ?do
      s" soft overflow" GPT-TIMEOUT-MS [: GPT-SOFT-ONE ;] GT-POOL-START-FORK
   loop ;

\ One child process runs every failure class; reds accumulate to 17 and the
\ final fail-closed drain lists every one of them.
: GPT-BATTERY-CASE ( -- )
   s" gate-pool-test-battery" GT-START
   2 GT-POOL-SLOTS!
   GT-POOL-RESET
   GPT-BATTERY-TRUNC
   GPT-BATTERY-SOFT
   GPT-BATTERY-WRAP
   GPT-BATTERY-TIMEOUT
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

: GPT-EXPECT-OVERFLOW-OUT ( n -- ) {: outu:n :}
   GPT-OUT outu s" red tests: 17" CONTAINS? TTRUE
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
   outu GPT-EXPECT-OVERFLOW-OUT
   erru GPT-EXPECT-BATTERY-ERR ;

: GPT-BIG-ALLOC ( -- )
   GPT-BIG-A-FIELD @ 0= if
      GPT-BIG-BYTES $400 + GT-POOL-ALLOC-BYTES GPT-BIG-A-FIELD !
   then ;

: GPT-BIG-FILE-LEN ( -- n )
   0 >IDX GT-POOL-OUT-FILE$ GPT-BIG-A-FIELD @ GPT-BIG-BYTES $400 + READ-ALL ;

: GPT-BIG-EXPECT ( -- )
   0 >IDX GT-POOL-OUT-U-PTR @ GT-OUT-CAP T=
   0 >IDX GT-POOL-OUT-TOTAL-PTR @ GPT-BIG-BYTES T=
   0 >IDX GT-POOL-OUT-FILE$ FILE? TTRUE
   GPT-BIG-FILE-LEN GPT-BIG-BYTES T=
   GPT-BIG-A-FIELD @ c@ $41 T=
   GPT-BIG-A-FIELD @ GPT-BIG-BYTES 1- BYTE+ c@ $41 T=
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
250 constant GPT-GK-TIMEOUT-MS
1500 constant GPT-GK-DELAY-MS
1500 constant GPT-GK-WAIT-MS

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

: GPT-GK-SENTINEL! ( -- )
   GT-ROOT s" group-kill-sentinel" GPT-GK-SENTINEL JOIN-PATH GPT-GK-SENTINEL-U ! ;

: GPT-GROUP-KILL-CASE ( -- )
   s" gate-pool-group-kill" GT-START
   GPT-GK-SENTINEL!
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   s" group-kill worker" GPT-GK-TIMEOUT-MS [: GPT-GK-CHILD ;] GT-POOL-START-FORK
   GT-POOL-DRAIN-SOFT
   GPT-GK-WAIT-MS GPT-SLEEP-MS
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
   [: GPT-WAIT-ANY ;] E-PROC-WAIT TTHROWSQ ;

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
   s" hb-fork-cleanup" TMPDIR-MKDIR GPT-FC-EV!
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

: GATE-POOL-TEST-MAIN ( -- )
   s" fail-battery-case" GPT-MODE? if GPT-BATTERY-CASE exit then
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
   GPT-BATTERY-REPORT
   T-REPORT
   s" gate-pool-test: ok" type cr ;

\ The battery runs at load time, so it is dispatched from top-level scope:
\ a package is still open above this line.
' GATE-POOL-TEST-MAIN
;package
execute

\ gate-signal-test.f - a stopped gate leaves no process and no scratch behind.
\
\     bin/hb --load test/gate-signal-test.f
\
\ THE SUBJECT is the gate itself. test/gate-signal-root.f is test/run.f's own
\ adapter on a one-row registry, started here as a child with a private HB_TMP.
\ Its row (test/gate-signal-row.f) runs a second engine, and that one starts a
\ burst of writers, each a process group of its own that writes under the
\ row's scratch and then sleeps 20 s. The test signals the root the moment both
\ engines have reported, so the signal lands while the burst is running.
\
\ THE WAYS THIS CAN FAIL, written down before the fix:
\
\  1. AN ORPHANED GRANDCHILD. Every spawned child leads its own process group
\     (docs/process-pty.md), so killing a row's group ends the row and nothing
\     the row spawned: its children go on building, reparented to init.
\     Asserted: every process under the root inherits the write end of one
\     pipe, and the read end reports end of file only when the last is gone.
\  2. A ROW MID-SPAWN. A child that appears after the row's descendants were
\     listed, or after its parent was killed, is nobody's to end.
\     Asserted: the leaf is inside its burst when the signal lands, and one
\     writer that got away holds the pipe open.
\  3. A SIGNAL DURING CLEANUP. A root that is ending its rows or removing its
\     tree and takes the default action for the next signal dies half way.
\     Asserted: the second-signal case below.
\  4. A SECOND SIGNAL. An impatient second SIGTERM lands while the first is
\     being answered; the outcome has to be the first one's.
\     Asserted: the same signal twice, AGAIN-GAP-MS apart, changes nothing.
\  5. SCRATCH IN USE. A tree removed while a process that can still write into
\     it lives comes back: every writer here makes its directory with
\     `mkdir -p` and then a file in it.
\     Asserted: the root's HB_TMP is empty when the root has ended and still
\     empty once every process is gone.
\  6. A STATUS THAT HIDES THE SIGNAL. A root that cleans up and then exits 0,
\     or 1 like a red run, tells its caller the run finished.
\     Asserted: the root's wait status is "killed by" the signal it was sent.
\  7. A DEADLINE KILL. The pool ends a row that outran its deadline with the
\     same kill, so the same children outlive it.
\     Asserted: the deadline case runs the row under a pool in this process,
\     retires it while its leaf is mid-burst, and makes claims 1, 2 and 5.
\  8. AN IGNORED SIGNAL. A root is started with its caller's ignored signals
\     still ignored: nohup's SIGHUP, the SIGINT of a background job under a
\     shell without job control. A root that catches one anyway undoes the
\     caller's choice, and a test that inherits one never sees it delivered.
\     Asserted: a root started under nohup takes no action on SIGHUP, and then
\     answers SIGTERM like any other. For every other case this process gives
\     its children the default action for all three before it starts a root.
\
\ A FAILED CASE LEAVES NOTHING EITHER, but for one case. Each case ends through
\ SIGNAL-END or DEADLINE-END whatever it asserted or threw: the root is killed
\ and reaped, the engines that reported are killed if they are still the
\ processes that reported, the writers nobody could name end on their own
\ after 20 s, and the case waits for that before it returns. The one case is a
\ root killed in the middle of a walk: what the walk had stopped stays stopped
\ unless the kernel resumes it (docs/gate.md), and a writer it does not resume
\ - the helper's, whose group was orphaned already - outlives the wait and is
\ left. The scratch is under this process's runner root, which its registered
\ cleanup removes. A passing case costs none of it: the pipe is already at end
\ of file.

require lib/errors.f
require lib/string.f
require lib/le.f
require lib/ffi-abi.f
require lib/fmt.f
require lib/test.f
require lib/fs.f
require lib/fs-list.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/signal.f
require lib/engine-candidate.f
require lib/test/runner.f
require test/gate-pool.f

package GATE-SIGNAL-TEST

60000 constant READY-MS              \ two engine boots under a loaded pool
10000 constant EXIT-MS               \ half the writers' 20 s: a root that waited them out is late
5000 constant GONE-MS                \ the killed take a moment to finish dying
30000 constant EXPIRE-MS             \ past the writers' 20 s, for a case that failed to end them
120000 constant ROW-MS               \ the deadline case's row, until the case retires it
5 constant AGAIN-GAP-MS              \ the second signal, while the first is being answered
500 constant IGNORED-MS              \ how long an ignored signal is given to stop the root after all
10 constant SETTLE-MS
2 constant REPORT-N                  \ the row's pid, then its leaf's
REPORT-N cells constant REPORT-BYTES
64 constant SINK-CAP
$8000 constant LOG-CAP
-1 constant NO-FD

create REPORTS REPORT-BYTES allot
create STARTS REPORT-BYTES allot     \ the start of each reported process, 0 for none
create SINK SINK-CAP allot
create LOG-TEXT LOG-CAP allot
create PAUSE-PFD 8 allot
create TMP FS-PATH-CAP allot
create LOG FS-PATH-CAP allot
create NOHUP FS-PATH-CAP allot

variable REPORT-U                    \ bytes of REPORTS read so far
variable PIPE-RD
variable PIPE-WR
variable ROOT-PID
variable ROOT-WATCH
variable CASE-SIG
variable TMP-U
variable LOG-U
variable ENTRY-N

TYPED-VARIABLE ROOT-LIVE bool        \ started and not yet reaped
TYPED-VARIABLE PIPE-DONE bool        \ the read end has reported end of file
TYPED-VARIABLE CASE-AGAIN bool       \ the case sends its signal twice
TYPED-VARIABLE CASE-NOHUP bool       \ the root is started under nohup and sent SIGHUP first
false ROOT-LIVE !
true PIPE-DONE !
false CASE-AGAIN !
false CASE-NOHUP !
NO-FD PIPE-RD !
NO-FD PIPE-WR !
NO-FD ROOT-WATCH !

: TMP$ ( -- ptr u8 n )    TMP TMP-U @ ;
: LOG$ ( -- ptr u8 n )    LOG LOG-U @ ;

: PAUSE ( n -- ) {: ms:n :}
   PAUSE-PFD 0 ms poll drop ;

: CLOSE-CELL ( ptr n -- ) {: p:ptr :}
   p @ 0 >= if p @ close then
   NO-FD p ! ;

\ Failure mode 8. RELEASE restores the default action for every signal CATCH
\ installed (docs/signal.md), whatever this process inherited for it.
: SIGNALS-DEFAULT ( -- )
   SIGNAL:INIT
   SIGNAL:SIGTERM SIGNAL:CATCH
   SIGNAL:SIGINT SIGNAL:CATCH
   SIGNAL:SIGHUP SIGNAL:CATCH
   SIGNAL:RELEASE ;

\ ---- the scratch ------------------------------------------------------------

\ TMP, under this test's runner root, is the HB_TMP a root is handed, so what
\ a root leaves is whatever TMP holds; the root's own output sits beside it.
: PATHS! ( -- )
   GT-ROOT s" tmp" TMP JOIN-PATH TMP-U !
   GT-ROOT s" root.log" LOG JOIN-PATH LOG-U !
   TMP$ MAKE-DIRS ;

: ENTRY+ ( ptr u8 n -- )
   2drop ENTRY-N @ 1+ ENTRY-N ! ;

: TMP-ENTRIES ( -- n )
   0 ENTRY-N !
   TMP$ [: ENTRY+ ;] FS-LIST:EACH
   ENTRY-N @ ;

: TMP-CLEAR ( -- )
   TMP$ REMOVE-TREE
   TMP$ MAKE-DIRS ;

\ ---- the report pipe --------------------------------------------------------

\ The write end stays inheritable: it is what every process under the subject
\ holds. The read end is this process's alone.
: OPEN-PIPE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   r FD>N PIPE-RD !
   w FD>N PIPE-WR !
   0 REPORT-U !
   REPORT-N 0 ?do 0 i cells STARTS + ! loop
   false PIPE-DONE ! ;

: FD-TEXT$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-INT SB$ ;

\ ---- who reported -----------------------------------------------------------
\
\ A reported pid names the engine that wrote it only while that engine lives.
\ Once it is reaped the number can go to a stranger, and other gates run on
\ this host, so a process is known by its pid AND its start, taken as soon as
\ the reports are in, and nothing is signalled or counted by pid alone.
\ macOS: proc_pidinfo's proc_bsdinfo (flavor 3, 136 bytes, sys/proc_info.h)
\ holds the start at 120 (seconds) and 128 (microseconds); measured here, a
\ zombie, a reaped pid and a missing one answer no record. Linux: field 22 of
\ /proc/<pid>/stat, in clock ticks since boot (proc(5)); a zombie keeps it.
\ Either way 0 is "no such process".

PROCESS-SYMBOLS
FUNCTION: PID-INFO proc_pidinfo ( n n n ptr u8 n -- i32 )
   3 4 WRITES-ARG
;FUNCTION

3 constant BSD-FLAVOR
136 constant BSD-BYTES
120 constant BSD-START-S
128 constant BSD-START-US
1000000 constant US-PER-S
512 constant STAT-CAP
20 constant STAT-START-FIELD         \ field 22, counted from the state after comm's `)`
$29 constant CLOSE-PAREN

create BSD BSD-BYTES allot
create STAT STAT-CAP allot
create STAT-PATH FS-PATH-CAP allot
variable STAT-PATH-U

: STARTED-MACOS ( n -- n ) {: pid:n :}
   pid BSD-FLAVOR 0 BSD BSD-BYTES PID-INFO BSD-BYTES <> if 0 exit then
   BSD BSD-START-S + LE:U64@ US-PER-S * BSD BSD-START-US + LE:U64@ + ;

: STAT-PATH+ ( ptr u8 n -- )
   STAT-PATH FS-PATH-CAP STAT-PATH-U BUF-APPEND ;

\ The first index from at, below end, that is not a space; then that is one.
: PAST-SPACES ( n n -- n ) {: at:n end:n :}
   at begin dup end < if STAT over + c@ STR-SPACE = else false then while 1+ repeat ;

: PAST-WORD ( n n -- n ) {: at:n end:n :}
   at begin dup end < if STAT over + c@ STR-SPACE <> else false then while 1+ repeat ;

\ comm may hold spaces and parentheses, so the fields are counted from after
\ the LAST `)`.
: STARTED-LINUX ( n -- n ) {: pid:n :}
   STAT-PATH-U BUF-RESET
   s" /proc/" STAT-PATH+
   pid FD-TEXT$ STAT-PATH+
   s\" /stat\z" STAT-PATH+
   STAT-PATH open-rd {: fd:n :}
   fd 0 < if 0 exit then
   fd STAT STAT-CAP read {: got:n :}
   fd close
   got 0 <= if 0 exit then
   -1 got 0 ?do STAT i + c@ CLOSE-PAREN = if drop i then loop {: close:n :}
   close 0 < if 0 exit then
   close 1+
   STAT-START-FIELD 1- 0 ?do got PAST-SPACES got PAST-WORD loop
   got PAST-SPACES {: start:n :}
   start got PAST-WORD {: stop:n :}
   STAT start + stop start - STR>NUMBER? MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

: STARTED ( pid -- n )
   PID>N
   HB-TARGET-MACOS? if STARTED-MACOS exit then
   STARTED-LINUX ;

: REPORT-ENV ( -- )
   s" GATE_SIGNAL_FD" >LEN PIPE-WR @ FD-TEXT$ >LEN PROC-ENV+ ;

\ TRUE once fd has bytes to read or has reached end of file, within ms.
: READY-WITHIN? ( n n -- bool ) {: fd:n ms:n :}
   fd >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

\ Both pids, before READY-MS runs out. End of file first means a reporter died.
: REPORTS-READ? ( -- bool )
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin REPORT-U @ REPORT-BYTES < while
      PIPE-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ REPORTS REPORT-U @ + REPORT-BYTES REPORT-U @ - read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file within ms: nothing under the subject holds the write end.
: GONE-WITHIN? ( n -- bool ) {: ms:n :}
   PIPE-DONE @ if true exit then
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      PIPE-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ SINK SINK-CAP read {: got:n :}
      got 0= if true PIPE-DONE ! true exit then
      got 0 < if false exit then
   again ;

: REPORTED ( n -- pid ) {: i:n :}
   i cells REPORTS + @ >PID ;

: REPORTED# ( -- n )
   REPORT-U @ 1 cells / ;

\ In, and each with a start: without one a reporter cannot be told from a
\ stranger, and every later claim about it would hold by default.
: REPORTS-IN? ( -- bool )
   REPORTS-READ?
   REPORTED# 0 ?do i REPORTED STARTED i cells STARTS + ! loop
   REPORTED# 0 ?do i cells STARTS + @ 0= if drop false then loop ;

\ Still the process that reported i: a start was taken, and the pid has it.
: REPORTED-SAME? ( n -- bool ) {: i:n :}
   i cells STARTS + @ {: was:n :}
   was 0<> i REPORTED STARTED was = and ;

\ A killed engine takes a moment to die, and on Linux its zombie keeps its
\ start until its new parent reaps it, so "gone" is asked again for a moment
\ rather than once.
: REPORTED-GONE? ( -- bool )
   GONE-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      true
      REPORTED# 0 ?do i REPORTED-SAME? if drop false then loop
      if true exit then
      deadline PROC-LEFT-MS MS>N 0= if false exit then
      SETTLE-MS PAUSE
   again ;

\ What a failed case owes: the engines that reported are killed, each only
\ while it is still the process that reported, and the writers nobody could
\ name have 20 s to live.
: SWEEP ( -- )
   REPORTED# 0 ?do
      i REPORTED-SAME? if i REPORTED SIGKILL PROC-KILL-RAW drop then
   loop
   s" the sweep left no process behind" T-LABEL
   EXPIRE-MS GONE-WITHIN? TTRUE ;

\ The two claims that outlive the subject, asked of whichever process ended the
\ row: nothing holds the pipe, and the engines that reported are gone.
: EXPECT-GONE ( -- )
   s" no process under the row is left" T-LABEL
   GONE-MS GONE-WITHIN? dup TTRUE
   if
      s" the engines that reported are gone" T-LABEL
      REPORTED-GONE? TTRUE
   then ;

\ ---- the gate root ----------------------------------------------------------

: OPEN-NULL ( -- n )
   s\" /dev/null\z" drop open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: OPEN-LOG ( -- n )
   LOG$ FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: NOHUP$ ( -- ptr u8 n )
   s" nohup" >LEN NOHUP FIND-EXECUTABLE MATCH option
      none OF s" gate-signal-test: required executable missing on PATH: nohup" 1 die ENDOF
      some OF LEN>N ENDOF
   ;MATCH
   NOHUP swap ;

\ Stages the root's argv and answers the executable: the engine, or nohup with
\ the engine as its first argument. nohup sets SIGHUP to ignored and becomes
\ the engine, so the pid spawned is the root's either way.
: ROOT-ARGV ( -- ptr u8 n )
   PROC-ARGV-ENV-RESET
   CASE-NOHUP @ if ENGINE-CANDIDATE:PATH$ >LEN PROC-ARGV+ then
   s" --load" >LEN PROC-ARGV+
   s" test/gate-signal-root.f" >LEN PROC-ARGV+
   CASE-NOHUP @ if NOHUP$ exit then
   ENGINE-CANDIDATE:PATH$ ;

\ The watch is opened while the root is alive: macOS cannot register a process
\ that has already exited (test/proc-watch-smoke.f).
: ROOT-START ( -- )
   OPEN-PIPE
   ROOT-ARGV {: exe:ptr exeu:n :}
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   REPORT-ENV
   PROC-ENV-INHERIT-MISSING
   OPEN-NULL OPEN-LOG {: in:n log:n :}
   exe exeu >LEN in >FD log >FD log >FD PROC-SPAWN-ARGV-ENV-IO
   PID>N ROOT-PID !
   true ROOT-LIVE !
   in close
   log close
   PIPE-WR CLOSE-CELL
   ROOT-PID @ proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then ROOT-WATCH ! ;

\ A second signal may find the root already gone, so the answer is not read.
: ROOT-SIGNAL ( n -- ) {: sig:n :}
   ROOT-PID @ >PID sig PROC-KILL-RAW drop ;

: ROOT-ENDED? ( -- bool )
   ROOT-WATCH @ EXIT-MS READY-WITHIN? ;

: ROOT-REAP ( -- outcome )
   ROOT-PID @ >PID PROC-WAIT-OUTCOME
   false ROOT-LIVE ! ;

: KILLED-BY? ( outcome n -- bool ) {: sig:n :}
   MATCH outcome
      exited OF drop false ENDOF
      signaled OF sig = ENDOF
      timeout OF false ENDOF
   ;MATCH ;

\ A root still running is ended here, and its rows with it: the pool's reapers
\ end a row when the pool dies. What it printed says why it was late; the
\ kill's own outcome is no claim of this test.
: ROOT-END ( -- )
   ROOT-LIVE @ 0= if exit then
   ROOT-PID @ >PID SIGKILL PROC-KILL-RAW drop
   ROOT-REAP PROC-OUTCOME>RC drop
   s" the late root's output:" type cr
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP READ-ALL type cr ;

\ Failure mode 8: SIGHUP is no signal to a root under nohup, so the root and
\ every process under it are still there IGNORED-MS later.
: EXPECT-IGNORED ( -- )
   SIGNAL:SIGHUP ROOT-SIGNAL
   s" ignored: SIGHUP did not end a root under nohup" T-LABEL
   ROOT-WATCH @ IGNORED-MS READY-WITHIN? TFALSE
   s" ignored: nor anything under it" T-LABEL
   0 GONE-WITHIN? TFALSE ;

: SIGNAL-BODY ( -- )
   ROOT-START
   s" signal: the row and its leaf came up" T-LABEL
   REPORTS-IN? TTRUE
   CASE-NOHUP @ if EXPECT-IGNORED then
   CASE-SIG @ ROOT-SIGNAL
   CASE-AGAIN @ if AGAIN-GAP-MS PAUSE CASE-SIG @ ROOT-SIGNAL then
   s" signal: the root ended inside its bound" T-LABEL
   ROOT-ENDED? dup TTRUE
   if
      s" signal: the root's status names the signal" T-LABEL
      ROOT-REAP CASE-SIG @ KILLED-BY? TTRUE
      s" signal: the scratch is gone when the root has ended" T-LABEL
      TMP-ENTRIES 0 T=
   then
   ROOT-END
   EXPECT-GONE
   s" signal: nothing put the scratch back" T-LABEL
   TMP-ENTRIES 0 T= ;

\ Whatever the body asserted or threw. Every step is a no-op for a case that
\ passed.
: SIGNAL-END ( -- )
   ROOT-END
   PIPE-DONE @ 0= if SWEEP then
   ROOT-WATCH CLOSE-CELL
   PIPE-WR CLOSE-CELL
   PIPE-RD CLOSE-CELL
   TMP-CLEAR ;

\ The signal the root is to answer, whether it is sent twice, and whether the
\ root runs under nohup.
: SIGNAL-CASE ( n bool bool -- )
   CASE-NOHUP ! CASE-AGAIN ! CASE-SIG !
   [: SIGNAL-BODY ;] [: SIGNAL-END ;] finally ;

\ ---- a row the pool kills at its deadline -----------------------------------

\ The row starts with a deadline it cannot reach and is retired by this case
\ once both engines have reported: a fixed short deadline would race the two
\ boots on a loaded host.
: DEADLINE-START ( -- )
   OPEN-PIPE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-signal-row.f" >LEN PROC-ARGV+
   REPORT-ENV
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ s" deadline row" ROW-MS GT-POOL-START
   PIPE-WR CLOSE-CELL ;

: DEADLINE-NOW ( -- )
   0 0 >IDX GT-POOL-TIMEOUT-PTR ! ;

: DEADLINE-BODY ( -- )
   DEADLINE-START
   s" deadline: the row and its leaf came up" T-LABEL
   REPORTS-IN? TTRUE
   DEADLINE-NOW
   GT-POOL-DRAIN-SOFT
   s" deadline: the pool retired the row as a timeout" T-LABEL
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-TIMED-OUT-PTR @ TTRUE
   EXPECT-GONE
   s" deadline: the row's scratch is gone and stays gone" T-LABEL
   0 >IDX GT-POOL-TMP$ EXISTS? TFALSE ;

: DEADLINE-END ( -- )
   GT-POOL-KILL-ALL
   PIPE-DONE @ 0= if SWEEP then
   PIPE-WR CLOSE-CELL
   PIPE-RD CLOSE-CELL ;

\ The pool is reset before the body: GT-POOL-KILL-ALL reads the slot table the
\ reset fills in.
: DEADLINE-CASE ( -- )
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET
   [: DEADLINE-BODY ;] [: DEADLINE-END ;] finally ;

public

: MAIN ( -- )
   T-RESET
   SIGNALS-DEFAULT
   s" gate-signal" GT-START
   PATHS!
   s" gate-signal: SIGTERM" type cr
   SIGNAL:SIGTERM false false SIGNAL-CASE
   s" gate-signal: SIGINT" type cr
   SIGNAL:SIGINT false false SIGNAL-CASE
   s" gate-signal: SIGHUP" type cr
   SIGNAL:SIGHUP false false SIGNAL-CASE
   s" gate-signal: SIGTERM twice" type cr
   SIGNAL:SIGTERM true false SIGNAL-CASE
   s" gate-signal: SIGHUP ignored, then SIGTERM" type cr
   SIGNAL:SIGTERM false true SIGNAL-CASE
   s" gate-signal: deadline" type cr
   DEADLINE-CASE
   GT-CLEANUP ;

;package

GATE-SIGNAL-TEST:MAIN
T-REPORT
s" gate-signal-test: ok" type cr

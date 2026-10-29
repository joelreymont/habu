\ gate-signal-test.f - a stopped gate leaves no process and no scratch behind.
\ The subject is test/gate-signal-root.f, test/run.f's adapter on a one-row
\ registry, started as a child with a private TMPDIR; its row
\ (test/gate-signal-row.f) runs a second engine that starts a burst of writers,
\ each its own process group, writing under the row's scratch and then sleeping
\ 20 s. The root is signalled once both engines have reported, mid-burst.

require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-fork.f
require src/lib/signal.f
require src/lib/engine-candidate.f
require src/lib/test/runner.f
require src/lib/test/fifo.f

package GATE-SIGNAL-TEST

60000 constant READY-MS              \ two engine boots under a loaded pool
10000 constant EXIT-MS               \ half the writers' 20 s: a root that waited them out is late
5000 constant GONE-MS                \ the killed take a moment to finish dying
30000 constant EXPIRE-MS             \ past the writers' 20 s, for a case that failed to end them
5 constant AGAIN-GAP-MS              \ the second signal, while the first is being answered
500 constant IGNORED-MS              \ how long an ignored signal is given to stop the root after all
10 constant SETTLE-MS
2 constant REPORT-N                  \ the row's pid, then its leaf's
REPORT-N cells constant REPORT-BYTES
$8000 constant LOG-CAP

create REPORTS REPORT-BYTES allot
create STARTS REPORT-BYTES allot     \ the start of each reported process, 0 for none
create LOG-TEXT LOG-CAP allot
create PAUSE-PFD 8 allot
create TMP PATH-CAP allot
create LOG PATH-CAP allot
create NOHUP PATH-CAP allot
create FIFO PATH-CAP allot

variable REPORT-U                    \ bytes of REPORTS read so far
variable FIFO-U
variable FIFO-RD
variable ROOT-PID
variable ROOT-WATCH
variable CASE-SIG
variable TMP-U
variable LOG-U
variable ENTRY-N

TYPED-VARIABLE ROOT-LIVE bool        \ started and not yet reaped
TYPED-VARIABLE FIFO-DONE bool        \ the read end has reported end of file
TYPED-VARIABLE CASE-AGAIN bool       \ the case sends its signal twice
TYPED-VARIABLE CASE-NOHUP bool       \ the root is started under nohup and sent SIGHUP first
false ROOT-LIVE !
true FIFO-DONE !
false CASE-AGAIN !
false CASE-NOHUP !
PROC-NO-FD FIFO-RD !
PROC-NO-FD ROOT-WATCH !

: TMP$ ( -- ptr u8 n )    TMP TMP-U @ ;
: LOG$ ( -- ptr u8 n )    LOG LOG-U @ ;

: PAUSE ( n -- ) {: ms:n :}
   PAUSE-PFD 0 ms poll drop ;

\ Failure mode 8. RELEASE restores the default action for every signal CATCH
\ installed, whatever this process inherited for it.
: SIGNALS-DEFAULT ( -- )
   SIGNAL:INIT
   SIGNAL:SIGTERM SIGNAL:CATCH
   SIGNAL:SIGINT SIGNAL:CATCH
   SIGNAL:SIGHUP SIGNAL:CATCH
   SIGNAL:RELEASE ;

\ ---- the scratch ------------------------------------------------------------

\ TMP, under this test's runner root, is the TMPDIR a root is handed, so what
\ a root leaves is whatever TMP holds; the root's own output sits beside it.
: PATHS! ( -- )
   GT-ROOT s" tmp" TMP FS:JOIN-PATH TMP-U !
   GT-ROOT s" root.log" LOG FS:JOIN-PATH LOG-U !
   TMP$ FS:MAKE-DIRS ;

: ENTRY+ ( ptr u8 n -- )
   2drop ENTRY-N @ 1+ ENTRY-N ! ;

: TMP-ENTRIES ( -- n )
   0 ENTRY-N !
   TMP$ [: ENTRY+ ;] FS-LIST:EACH
   ENTRY-N @ ;

: TMP-CLEAR ( -- )
   TMP$ FS:REMOVE-TREE
   TMP$ FS:MAKE-DIRS ;

\ ---- the report FIFO --------------------------------------------------------

: FIFO$ ( -- ptr u8 n )
   FIFO FIFO-U @ ;

\ A new FIFO for the case, its read end open before the row starts, so the
\ row's open of the write end does not wait.
: OPEN-FIFO ( -- )
   GT-ROOT FIFO TEST-FIFO:MAKE {: u:n r:fd :}
   u FIFO-U !
   r FD>N FIFO-RD !
   0 REPORT-U !
   REPORT-N 0 ?do 0 i cells STARTS + ! loop
   false FIFO-DONE ! ;

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
create STAT-PATH PATH-CAP allot
variable STAT-PATH-U

: STARTED-MACOS ( n -- n ) {: pid:n :}
   pid BSD-FLAVOR 0 BSD BSD-BYTES PID-INFO BSD-BYTES <> if 0 exit then
   BSD BSD-START-S + LE:U64@ US-PER-S * BSD BSD-START-US + LE:U64@ + ;

: STAT-PATH+ ( ptr u8 n -- )
   STAT-PATH PATH-CAP STAT-PATH-U BUF-APPEND ;

\ The first index from at, below end, that is not a space; then that is one.
: PAST-SPACES ( n n -- n ) {: at:n end:n :}
   at begin dup end < if STAT over + c@ STR-SPACE = else false then while 1+ repeat ;

: PAST-WORD ( n n -- n ) {: at:n end:n :}
   at begin dup end < if STAT over + c@ STR-SPACE <> else false then while 1+ repeat ;

\ comm may hold spaces and parentheses, so the fields are counted from after
\ the LAST `)`.
: STARTED-LINUX ( n -- n )
   {: pid:n :}
   STAT-PATH-U BUF-RESET
   s" /proc/" STAT-PATH+
   pid FD-TEXT$ STAT-PATH+
   s" /stat" STAT-PATH+
   STAT-PATH STAT-PATH-U BUF-LEN@ FS:>PATHZ open-rd {: fd:n :}
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
   s" GATE_SIGNAL_FIFO" >LEN FIFO$ >LEN PROC-ENV+ ;

\ A wait that runs out is no assertion result: TEST-WAIT:EXPIRED names it on
\ stdout and throws E-PROC-TIMEOUT (src/lib/test/wait.f), so the row ends as a
\ timeout, not a FAIL.

\ TRUE once fd has bytes to read or has reached end of file, FALSE when ms ran
\ out first. A failed poll, or a descriptor the poll reports broken (POLLERR or
\ POLLNVAL), is no answer either way: it throws E-PROC-OUTPUT.
: READY-WITHIN? ( n n -- bool )
   {: fd:n ms:n :}
   fd >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   0 >IDX PROC-PFD-REVENTS POLLERR POLLNVAL or and 0 <> if E-PROC-OUTPUT throw then
   rc 0 > ;

\ Both pids, before READY-MS runs out. End of file first means a reporter died:
\ that, a short report or a failed read answers false. READY-MS running out
\ first is this wait's own deadline, named and thrown as a timeout.
\ On macOS, whose poll never reports a FIFO's end of file, a reporter that dies
\ before its report is this timeout instead.
: REPORTS-READ? ( -- bool )
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin REPORT-U @ REPORT-BYTES < while
      FIFO-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if
         s" gate-signal-test REPORTS-READ" TEST-WAIT:EXPIRED
      then
      FIFO-RD @ REPORTS REPORT-U @ + REPORT-BYTES REPORT-U @ - read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file within ms: nothing under the subject holds the write end. FALSE
\ when ms ran out first, which the zero-time probe of an ignored signal
\ expects. A failed read or poll throws E-PROC-OUTPUT.
: GONE-WITHIN? ( n -- bool )
   {: ms:n :}
   FIFO-DONE @ if true exit then
   FIFO-RD @ >FD ms TEST-FIFO:EOF-WITHIN {: eof:n :}
   eof 0 < if E-PROC-OUTPUT throw then
   eof 0= if false exit then
   true FIFO-DONE !
   true ;

\ Returns at end of file within ms. ms running out first is this wait's own
\ deadline, named and thrown as a timeout; a failed poll or read is no
\ deadline and throws E-PROC-OUTPUT.
: GONE-WITHIN ( n -- )
   GONE-WITHIN? 0= if s" gate-signal-test GONE-WITHIN" TEST-WAIT:EXPIRED then ;

: REPORTED ( n -- pid ) {: i:n :}
   i cells REPORTS + @ >PID ;

: REPORTED# ( -- n )
   REPORT-U @ 1 cells / ;

\ A FIFO no writer has opened yet reads end of file too, so a case judges end
\ of file only after both reports show the row and its leaf hold the write end.
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

\ Returns once no engine that reported is still the process that reported. A
\ killed engine takes a moment to die, and on Linux its zombie keeps its start
\ until its new parent reaps it, so "gone" is asked again for a moment rather
\ than once. GONE-MS running out first is this wait's own deadline, named and
\ thrown as a timeout.
: REPORTED-GONE ( -- )
   GONE-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      true
      REPORTED# 0 ?do i REPORTED-SAME? if drop false then loop
      if exit then
      deadline PROC-LEFT-MS MS>N 0= if
         s" gate-signal-test REPORTED-GONE" TEST-WAIT:EXPIRED
      then
      SETTLE-MS PAUSE
   again ;

\ A failed case leaves nothing behind, except after a root killed in the middle
\ of a walk: what the walk stopped stays stopped unless the kernel resumes it
\ (test/gate-pool.f header), and a writer it does not resume, the helper's,
\ whose group was orphaned already, outlives the wait and is left.
\ The scratch is under this process's runner root, which its registered cleanup
\ removes. A passing case skips the wait: its FIFO is already at end of file.

\ What a failed case owes: the engines that reported are killed, each only
\ while it is still the process that reported, and the writers nobody could
\ name have 20 s to live. EXPIRE-MS running out first is this wait's own
\ deadline, named and thrown as a timeout.
: SWEEP ( -- )
   REPORTED# 0 ?do
      i REPORTED-SAME? if i REPORTED SIGKILL PROC-KILL-RAW drop then
   loop
   EXPIRE-MS GONE-WITHIN ;

\ The two claims that outlive the subject, asked of whichever process ended the
\ row: nothing holds the FIFO, and the engines that reported are gone. Each is
\ a wait, and its deadline running out is a timeout.
: EXPECT-GONE ( -- )
   GONE-MS GONE-WITHIN
   REPORTED-GONE ;

\ ---- the gate root ----------------------------------------------------------

: OPEN-NULL ( -- n )
   s\" /dev/null\z" drop open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: OPEN-LOG ( -- n )
   LOG$ FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open {: fd:n :}
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
\ that has already exited.
: ROOT-START ( -- )
   OPEN-FIFO
   ROOT-ARGV {: exe:ptr exeu:n :}
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   REPORT-ENV
   PROC-ENV-INHERIT-MISSING
   OPEN-NULL OPEN-LOG {: in:n log:n :}
   exe exeu >LEN in >FD log >FD log >FD PROC-SPAWN-ARGV-ENV-IO
   PID>N ROOT-PID !
   true ROOT-LIVE !
   in close
   log close
   ROOT-PID @ proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then ROOT-WATCH ! ;

\ A second signal may find the root already gone, so the answer is not read.
: ROOT-SIGNAL ( n -- ) {: sig:n :}
   ROOT-PID @ >PID sig PROC-KILL-RAW drop ;

\ Returns once the root has ended. EXIT-MS running out first is this wait's own
\ deadline, named and thrown as a timeout; a failed poll throws E-PROC-OUTPUT.
: ROOT-ENDED ( -- )
   ROOT-WATCH @ EXIT-MS READY-WITHIN? 0= if
      s" gate-signal-test ROOT-ENDED" TEST-WAIT:EXPIRED
   then ;

: ROOT-REAP ( -- outcome )
   ROOT-PID @ >PID PROC-WAIT-OUTCOME
   false ROOT-LIVE ! ;

\ PROC-WAIT-OUTCOME never makes `timeout`: its wait blocks until the child ends.
\ The arm throws, as PROC-OUTCOME>RC's does, so a deadline wait fed here later
\ cannot read as no kill.
: KILLED-BY? ( outcome n -- bool ) {: sig:n :}
   MATCH outcome
      exited OF drop false ENDOF
      signaled OF sig = ENDOF
      timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

\ A root still running is ended here, and its rows with it: the pool's reapers
\ end a row when the pool dies. What it printed says why it was late; the
\ kill's own outcome is no claim of this test.
: ROOT-END ( -- )
   ROOT-LIVE @ 0= if exit then
   ROOT-PID @ >PID SIGKILL PROC-KILL-RAW drop
   ROOT-REAP PROC-OUTCOME>RC drop
   s" the late root's output:" type cr
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL type cr ;

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
   ROOT-ENDED
   s" signal: the root's status names the signal" T-LABEL
   ROOT-REAP CASE-SIG @ KILLED-BY? TTRUE
   s" signal: the scratch is gone when the root has ended" T-LABEL
   TMP-ENTRIES 0 T=
   EXPECT-GONE
   s" signal: nothing put the scratch back" T-LABEL
   TMP-ENTRIES 0 T= ;

\ Whatever the body asserted or threw. Every step is a no-op for a case that
\ passed. The sweep runs even when ending the root throws, and the descriptors
\ close and the scratch clears even when the sweep's wait throws.
: SIGNAL-END ( -- )
   [: [: ROOT-END ;]
      [: FIFO-DONE @ 0= if SWEEP then ;] finally ;]
   [: ROOT-WATCH PROC-CLOSE-CELL
      FIFO-RD PROC-CLOSE-CELL
      TMP-CLEAR ;] finally ;

\ The signal the root is to answer, whether it is sent twice, and whether the
\ root runs under nohup.
: SIGNAL-CASE ( n bool bool -- )
   CASE-NOHUP ! CASE-AGAIN ! CASE-SIG !
   [: SIGNAL-BODY ;] [: SIGNAL-END ;] finally ;

public

\ The ways this can fail, each asserted by a case MAIN runs:
\  1. An orphaned grandchild. Every spawned child leads its own process group
\     (src/lib/process-tree.f), so killing a row's group leaves what the row
\     spawned running. The row and its leaf open the write end of a FIFO
\     (src/lib/test/fifo.f) at the path GATE_SIGNAL_FIFO names, the leaf maps it
\     into each process it spawns, and the read end reports end of file only
\     when the last holder is gone. The root holds no write end: its wait
\     status is the evidence it ended.
\  2. A row mid-spawn. A child that appears after the row's descendants were
\     listed, or after its parent was killed, is nobody's to end. The leaf is
\     inside its burst when the signal lands, and one writer that got away
\     holds the FIFO open.
\  3. A signal during cleanup. A root ending its rows or removing its tree that
\     takes the default action for the next signal dies half way.
\  4. A second signal. SIGTERM twice, AGAIN-GAP-MS apart, has the first one's
\     outcome.
\  5. Scratch in use. A tree removed while a writer lives comes back: every
\     writer makes its directory with `mkdir -p` and then a file in it.
\  6. A status that hides the signal. A root that cleans up and then exits 0,
\     or 1 like a red run, tells its caller the run finished.
\  7. An ignored signal. A root starts with its caller's ignored signals still
\     ignored: nohup's SIGHUP, the SIGINT of a background job under a shell
\     without job control. A root that catches one undoes the caller's choice,
\     and a test that inherits one never sees it delivered. A root under nohup
\     takes no action on SIGHUP, then answers SIGTERM; every other case gives
\     its children the default action for all three before it starts a root.
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
   GT-CLEANUP ;

;package

GATE-SIGNAL-TEST:MAIN
T-REPORT
s" gate-signal-test: ok" type cr

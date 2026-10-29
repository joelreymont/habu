\ check-signal-test.f - a stopped check.f, and a check run past its deadline,
\ leave no process and no scratch behind.
\ Runs tools/check.f on test/check-signal-subject.f as a child with a private
\ TMPDIR; check.f's run stage runs the subject in a child of its own, which
\ starts a sleeper and reports. Each case signals check.f once the report is in,
\ or gives it a deadline the subject's wait on its sleeper outlasts.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/signal.f
require src/lib/engine-candidate.f
require src/lib/test/runner.f
require src/lib/test/wait.f
require src/lib/test/fifo.f
require src/core/include.f               \ SOURCE-ROOT:CANON-OS, the name --json-errors gives a file

package CHECK-SIGNAL-TEST

\ This test's own waits (for the report, for room in check.f's stdin, for
\ check.f to end, for end of file on the FIFO) are named on stdout and thrown as
\ E-PROC-TIMEOUT (src/lib/test/wait.f), so the row ends as a timeout, not a
\ fail.
60000 constant READY-MS              \ check.f's static pass and two engine boots under a loaded pool
10000 constant EXIT-MS               \ half the sleeper's 20 s: a check.f that waited it out is late
5000 constant GONE-MS                \ the killed take a moment to finish dying
5000 constant DEADLINE-MS            \ the subject reported 0.6 s after check.f began, at load 15; the sleeper outlasts it
30000 constant EXPIRE-MS             \ past the sleeper's 20 s, for a case that failed to end it
1 cells constant REPORT-BYTES        \ the subject's pid
512 constant FEED-CHUNK              \ PIPE_BUF on macOS: a write poll admitted cannot block
$20000 constant PAD-BYTES            \ twice the 64 KiB a pipe holds on either host
$8000 constant LOG-CAP

create REPORT REPORT-BYTES allot
create LOG-TEXT LOG-CAP allot
create TMP PATH-CAP allot
create LOG PATH-CAP allot
create FIFO PATH-CAP allot
create FEED FEED-CHUNK allot         \ newlines: blank lines to check.f

variable REPORT-U                    \ bytes of REPORT read so far
variable FIFO-U
variable FIFO-RD
variable CHECK-PID
variable CHECK-WATCH
variable FEED-WR                     \ the write end of check.f's stdin
variable TMP-U
variable LOG-U
variable ENTRY-N

TYPED-VARIABLE CHECK-LIVE bool       \ started and not yet reaped
TYPED-VARIABLE FIFO-DONE bool        \ the read end has reported end of file
TYPED-VARIABLE CASE-CLOSED bool      \ the subject closes its output before it reports
false CHECK-LIVE !
true FIFO-DONE !
false CASE-CLOSED !
PROC-NO-FD FIFO-RD !
PROC-NO-FD CHECK-WATCH !
PROC-NO-FD FEED-WR !

: TMP$ ( -- ptr u8 n )    TMP TMP-U @ ;
: LOG$ ( -- ptr u8 n )    LOG LOG-U @ ;
: FIFO$ ( -- ptr u8 n )   FIFO FIFO-U @ ;

\ A process is started with its caller's ignored signals still ignored, and a
\ check.f that inherited one would never see it. RELEASE restores the default
\ action for every signal CATCH installed.
: SIGNALS-DEFAULT ( -- )
   SIGNAL:INIT
   SIGNAL:SIGTERM SIGNAL:CATCH
   SIGNAL:SIGINT SIGNAL:CATCH
   SIGNAL:SIGHUP SIGNAL:CATCH
   SIGNAL:RELEASE ;

\ TMP, under this test's runner root, is the TMPDIR check.f is handed, so what
\ check.f leaves is whatever TMP holds; its output sits beside it.
: PATHS! ( -- )
   GT-ROOT s" tmp" TMP FS:JOIN-PATH TMP-U !
   GT-ROOT s" check.log" LOG FS:JOIN-PATH LOG-U !
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

\ A new FIFO for the case, its read end open before check.f starts, so the
\ subject's open of the write end does not wait.
: OPEN-FIFO ( -- )
   GT-ROOT FIFO TEST-FIFO:MAKE {: u:n r:fd :}
   u FIFO-U !
   r FD>N FIFO-RD !
   0 REPORT-U !
   false FIFO-DONE ! ;

: FD-TEXT$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-INT SB$ ;

\ fd within ms: a positive count once it answers one of the poll events, 0 when
\ ms ran out first, a negative errno when the poll failed.
: POLL-WITHIN ( n n n -- n )
   {: fd:n events:n ms:n :}
   fd >FD events PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART ;

\ fd within ms, answered as POLL-WITHIN answers: ready once it has bytes to read
\ or has reached end of file. A descriptor the poll reports broken (POLLERR or
\ POLLNVAL) is no answer either way: it throws E-PROC-OUTPUT.
: READY-WITHIN ( n n -- n )
   POLLIN swap POLL-WITHIN {: ready:n :}
   0 >IDX PROC-PFD-REVENTS POLLERR POLLNVAL or and 0 <> if E-PROC-OUTPUT throw then
   ready ;

\ The subject's pid, before READY-MS runs out. End of file first means check.f
\ or its child died before the subject ran: that, a short report or a failed
\ read or poll answers false. READY-MS running out first is this test's own
\ deadline, named and thrown as a timeout. On macOS, whose poll never reports a
\ FIFO's end of file, a subject that dies before its report is that timeout.
: REPORT-READ? ( -- bool )
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin REPORT-U @ REPORT-BYTES < while
      FIFO-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN {: ready:n :}
      ready 0= if s" check-signal-test REPORT-READ" TEST-WAIT:EXPIRED then
      ready 0 < if false exit then
      FIFO-RD @ REPORT REPORT-U @ + REPORT-BYTES REPORT-U @ - read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file within ms: nothing under check.f holds the write end. A failed
\ read or poll answers false. ms running out first is this test's own
\ deadline, named and thrown as a timeout. A FIFO no writer has opened reads end
\ of file too, so it means anything only after the subject's report.
: GONE-WITHIN? ( n -- bool )
   {: ms:n :}
   FIFO-DONE @ if true exit then
   FIFO-RD @ >FD ms TEST-FIFO:EOF-WITHIN {: eof:n :}
   eof 0= if s" check-signal-test GONE-WITHIN" TEST-WAIT:EXPIRED then
   eof 0 < if false exit then
   true FIFO-DONE !
   true ;

: OPEN-NULL ( -- n )
   s\" /dev/null\z" drop open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: OPEN-LOG ( -- n )
   LOG$ FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: CHECK-BEGIN ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+ ;

\ check.f, with the case's arguments and environment added and in as its stdin.
\ The watch is opened while check.f is alive: macOS cannot register a process
\ that has already exited.
: CHECK-SPAWN ( n -- ) {: in:n :}
   s" TMPDIR" >LEN TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   OPEN-LOG {: log:n :}
   ENGINE-CANDIDATE:PATH$ >LEN in >FD log >FD log >FD PROC-SPAWN-ARGV-ENV-IO
   PID>N CHECK-PID !
   true CHECK-LIVE !
   in close
   log close
   CHECK-PID @ proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then CHECK-WATCH ! ;

: SUBJECT$ ( -- ptr u8 n )
   s" test/check-signal-subject.f" ;

\ The subject and its report FIFO, after whatever options check.f is given
\ ahead of it.
: SUBJECT-START ( -- )
   OPEN-FIFO
   SUBJECT$ >LEN PROC-ARGV+
   s" CHECK_SIGNAL_FIFO" >LEN FIFO$ >LEN PROC-ENV+
   CASE-CLOSED @ if s" CHECK_SIGNAL_CLOSED" >LEN s" 1" >LEN PROC-ENV+ then
   OPEN-NULL CHECK-SPAWN ;

: CHECK-START ( -- )
   CHECK-BEGIN
   s" --" >LEN PROC-ARGV+
   SUBJECT-START ;

\ check.f with a deadline of DEADLINE-MS, in --json-errors mode or its default.
: LATE-START ( bool -- ) {: json:bool :}
   CHECK-BEGIN
   s" --" >LEN PROC-ARGV+
   s" --deadline-ms" >LEN PROC-ARGV+
   DEADLINE-MS FD-TEXT$ >LEN PROC-ARGV+
   json if s" --json-errors" >LEN PROC-ARGV+ then
   SUBJECT-START ;

\ check.f with no input named reads its program from stdin, a pipe this test
\ writes. The write end is this process's alone, or check.f would hold its own
\ stdin open.
: CHECK-START-STDIN ( -- )
   CHECK-BEGIN
   PIPE-PAIR {: r:fd w:fd :}
   w FD-CLOEXEC!
   w FD-NOSIGPIPE!
   w FD>N FEED-WR !
   r FD>N CHECK-SPAWN ;

\ u bytes at a into check.f's stdin once the pipe has room for them, before
\ the deadline. u is at most FEED-CHUNK, so the write cannot block. A short
\ write or a failed poll answers false. The deadline running out first is this
\ test's own, named and thrown as a timeout.
: FEED? ( ptr u8 n n -- bool )
   {: a:ptr u:n deadline:n :}
   FEED-WR @ POLLOUT deadline PROC-LEFT-MS MS>N POLL-WITHIN {: ready:n :}
   ready 0= if s" check-signal-test FEED" TEST-WAIT:EXPIRED then
   ready 0 < if false exit then
   FEED-WR @ a u write u = ;

: FEED-FILL ( -- )
   FEED-CHUNK 0 ?do 10 FEED i + c! loop ;

\ More blank lines than the pipe holds: the last of them goes in only once
\ check.f reads its program, which it does after it has caught the stops.
: FEED-PAD? ( -- bool )
   FEED-FILL
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   PAD-BYTES FEED-CHUNK / 0 ?do
      FEED FEED-CHUNK deadline FEED? 0= if false unloop exit then
   loop
   true ;

\ A definition that leaves nothing for its declared n, then end of file.
: FEED-REFUSED? ( -- bool )
   s" : BAD ( -- n ) ;" READY-MS >MS PROC-DEADLINE-AT FEED?
   FEED-WR PROC-CLOSE-CELL ;

: CHECK-SIGNAL ( n -- ) {: sig:n :}
   CHECK-PID @ >PID sig PROC-KILL-RAW drop ;

\ Returns once check.f has ended, within ms. ms running out first is this
\ test's own deadline, named and thrown as a timeout; a failed poll of the
\ watch, or one that reports it broken, throws E-PROC-OUTPUT, a failure.
: CHECK-ENDED-WITHIN ( n -- )
   {: ms:n :}
   CHECK-WATCH @ ms READY-WITHIN {: ready:n :}
   ready 0= if s" check-signal-test CHECK-ENDED" TEST-WAIT:EXPIRED then
   ready 0 < if E-PROC-OUTPUT throw then ;

\ Returns once check.f has ended, inside the EXIT-MS a signal's answer gets.
: CHECK-ENDED ( -- )
   EXIT-MS CHECK-ENDED-WITHIN ;

: CHECK-REAP ( -- outcome )
   CHECK-PID @ >PID PROC-WAIT-OUTCOME
   false CHECK-LIVE ! ;

\ PROC-WAIT-OUTCOME never makes `timeout`: its wait blocks until the child ends.
\ The arm throws, as PROC-OUTCOME>RC's does, so a deadline wait fed here later
\ cannot read as no kill.
: KILLED-BY? ( outcome n -- bool ) {: sig:n :}
   MATCH outcome
      exited OF drop false ENDOF
      signaled OF sig = ENDOF
      timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

\ A check.f still running is ended here. What it printed says why it was late;
\ the kill's own outcome is no claim of this test.
: CHECK-END ( -- )
   CHECK-LIVE @ 0= if exit then
   CHECK-PID @ >PID SIGKILL PROC-KILL-RAW drop
   CHECK-REAP PROC-OUTCOME>RC drop
   s" the late check.f's output:" type cr
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL type cr ;

\ check.f names each step of its answer that threw (tools/check-core.f
\ CHK-SAY-THROW), and src/lib/process.f a walk of the child's tree it could not
\ finish (PROC-WALK-REPORT).
: LOG-THREW? ( -- bool )
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL {: a:ptr u:n :}
   a u s" threw" CONTAINS? a u s" not walked" CONTAINS? or ;

\ The ways a stopped check.f can leak, each asserted here:
\ - an orphaned child: check.f's child leads its own process group, so a
\   check.f that dies of the signal, or a `timeout` that kills its group, leaves
\   the child under init. The subject opens the write end of the FIFO named by
\   CHECK_SIGNAL_FIFO (src/lib/test/fifo.f); check.f, this test's child, holds
\   none, and its wait status is the evidence it ended;
\ - an orphaned grandchild: a kill of the child's group misses the sleeper,
\   which leads a group of its own; the subject maps the FIFO into it;
\ - scratch left: check.f's temporary directory, holding the run stage's run.f,
\   is under TMPDIR, which must be empty once check.f has ended;
\ - a status that hides the signal: a check.f that cleans up and exits tells its
\   caller the check finished; its wait status must be killed by the signal;
\ - a wait that does not hear it: check.f waits in a capture of its child's
\   output, and for a child that closed its output in the reap after it; each
\   wait has a case (CASE-CLOSED), and each waits EXIT-MS for check.f to end.
: CASE-BODY ( n -- )
   {: sig:n :}
   CHECK-START
   s" signal: the subject came up under check.f" T-LABEL
   REPORT-READ? TTRUE
   sig CHECK-SIGNAL
   CHECK-ENDED
   s" signal: check.f's status names the signal" T-LABEL
   CHECK-REAP sig KILLED-BY? TTRUE
   s" signal: no step of the answer threw" T-LABEL
   LOG-THREW? TFALSE
   s" signal: the scratch is gone when check.f has ended" T-LABEL
   TMP-ENTRIES 0 T=
   CHECK-END
   s" signal: no process under check.f is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE ;

\ check.f answers a stop after its run however the run ended, and a refused
\ program ends before any child is started: its capture row names pid 0, and a
\ kill of pid 0 kills check.f's own process group. SIGTERM lands while check.f
\ reads its program, so it answers once the program is refused. Nothing under
\ check.f could open a FIFO: this case makes none and asserts check.f's status.
: STDIN-BODY ( -- )
   CHECK-START-STDIN
   s" stdin: check.f read past what the pipe holds" T-LABEL
   FEED-PAD? TTRUE
   SIGNAL:SIGTERM CHECK-SIGNAL
   s" stdin: check.f took the refused program" T-LABEL
   FEED-REFUSED? TTRUE
   CHECK-ENDED
   s" stdin: check.f's status names the signal" T-LABEL
   CHECK-REAP SIGNAL:SIGTERM KILLED-BY? TTRUE
   s" stdin: no step of the answer threw" T-LABEL
   LOG-THREW? TFALSE
   s" stdin: the scratch is gone when check.f has ended" T-LABEL
   TMP-ENTRIES 0 T= ;

\ The one line check.f ends a run past its deadline with. It names the subject
\ as given, or canonically under --json-errors, as each of its reports does.
: LATE-LINE$ ( bool -- ptr u8 n ) {: json:bool :}
   SB-RESET
   s" check.f: " SB-APPEND
   json if SUBJECT$ SOURCE-ROOT:CANON-OS drop else SUBJECT$ then SB-APPEND
   s" : the run passed its deadline of " SB-APPEND
   DEADLINE-MS FMT:SB-INT
   s\"  ms\n" SB-APPEND
   SB$ ;

\ The capture that waits on check.f's child kills it at the deadline and throws
\ E-PROC-TIMEOUT. Uncaught, that is exit 67 and a bare code, and its kill ends
\ the child alone, leaving the sleeper under init. check.f must exit 70, its
\ refusal, with one line naming the subject and the deadline, and leak nothing:
\ in its default mode with the run's output open and in --json-errors with it
\ closed, so the deadline passes in each wait. check.f counts the deadline from
\ the run's start, before the report, so after the report it ends inside the
\ deadline and the EXIT-MS a signal's answer gets.
: LATE-BODY ( bool -- )
   {: json:bool :}
   json LATE-START
   s" deadline: the subject came up under check.f" T-LABEL
   REPORT-READ? TTRUE
   DEADLINE-MS EXIT-MS + CHECK-ENDED-WITHIN
   s" deadline: check.f exits its refusal status" T-LABEL
   CHECK-REAP PROC-OUTCOME>RC RC>N 70 T=
   s" deadline: one line names the subject and the deadline" T-LABEL
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL json LATE-LINE$ T$=
   s" deadline: the scratch is gone when check.f has ended" T-LABEL
   TMP-ENTRIES 0 T=
   CHECK-END
   s" deadline: no process under check.f is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE ;

: LEFT-ENDED ( -- )
   FIFO-DONE @ 0= if
      s" the processes check.f left ended on their own" T-LABEL
      EXPIRE-MS GONE-WITHIN? TTRUE
   then ;

\ Runs after every body, whatever it asserted or threw; each step is a no-op for
\ a case that passed. A check.f still running is killed and reaped, and the wait
\ for what it left (bounded by the sleeper's 20 s) runs even when that throws;
\ every descriptor closes and the scratch clears even when the wait throws. With
\ its read end closed the FIFO is done, so a case that makes none waits on no
\ earlier case's. The scratch is under this process's runner root, which its
\ registered cleanup removes.
: CASE-END ( -- )
   [: [: CHECK-END ;]
      [: LEFT-ENDED ;] finally ;]
   [: CHECK-WATCH PROC-CLOSE-CELL
      FEED-WR PROC-CLOSE-CELL
      FIFO-RD PROC-CLOSE-CELL
      true FIFO-DONE !
      TMP-CLEAR ;] finally ;

\ The signal check.f is to answer, and whether the subject closes its output.
: SIGNAL-CASE ( n bool -- ) {: sig:n closed:bool :}
   closed CASE-CLOSED !
   sig [: CASE-BODY ;] [: CASE-END ;] finally ;

\ Whether check.f runs in --json-errors mode, and whether the subject closes its
\ output.
: LATE-CASE ( bool bool -- ) {: json:bool closed:bool :}
   closed CASE-CLOSED !
   json [: LATE-BODY ;] [: CASE-END ;] finally ;

public

: MAIN ( -- )
   T-RESET
   SIGNALS-DEFAULT
   s" check-signal" GT-START
   PATHS!
   s" check-signal: SIGTERM" type cr
   SIGNAL:SIGTERM false SIGNAL-CASE
   s" check-signal: SIGINT" type cr
   SIGNAL:SIGINT false SIGNAL-CASE
   s" check-signal: SIGHUP" type cr
   SIGNAL:SIGHUP false SIGNAL-CASE
   s" check-signal: SIGTERM, the child's output closed" type cr
   SIGNAL:SIGTERM true SIGNAL-CASE
   s" check-signal: SIGTERM before any child, a refused program on stdin" type cr
   [: STDIN-BODY ;] [: CASE-END ;] finally
   s" check-signal: a run past its deadline" type cr
   false false LATE-CASE
   s" check-signal: a run past its deadline, --json-errors, the child's output closed" type cr
   true true LATE-CASE
   GT-CLEANUP ;

;package

CHECK-SIGNAL-TEST:MAIN
T-REPORT
s" check-signal-test: ok" type cr

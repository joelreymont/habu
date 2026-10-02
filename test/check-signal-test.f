\ check-signal-test.f - a stopped check.f, and a check run past its deadline,
\ leave no process and no scratch behind.
\
\     bin/hb --load test/check-signal-test.f
\
\ THE SUBJECT is tools/check.f checking test/check-signal-subject.f, started
\ here as a child with a private HB_TMP. check.f's run stage runs the subject in
\ a child of its own, and the subject starts a sleeper and reports. The test
\ signals check.f the moment the report is in, so the signal lands while check.f
\ waits on its child; or it gives check.f a deadline the subject's wait on its
\ sleeper outlasts, so the deadline passes while check.f waits.
\
\ THE WAYS THIS CAN FAIL, written down before the fix:
\
\  1. AN ORPHANED CHILD. check.f's child leads its own process group, so a
\     check.f that dies of the signal, or a `timeout` that kills check.f's
\     group, leaves the child running under init. Asserted: every process under
\     check.f inherits the write end of one pipe, and the read end reports end
\     of file only when the last is gone.
\  2. AN ORPHANED GRANDCHILD. A kill of the child's group ends the child and
\     not what it spawned: the sleeper leads a group of its own. Asserted by the
\     same pipe, which the sleeper holds.
\  3. SCRATCH LEFT. check.f's temporary directory, holding the run stage's
\     run.f, is made under HB_TMP. Asserted: HB_TMP is empty once check.f has
\     ended.
\  4. A STATUS THAT HIDES THE SIGNAL. A check.f that cleans up and then exits
\     with a status tells its caller the check finished. Asserted: its wait
\     status is "killed by" the signal it was sent.
\  5. A WAIT THAT DOES NOT HEAR IT. check.f waits in a capture of its child's
\     output, and a signal that capture never answers would hold check.f to its
\     deadline. A child that closed its output is waited for in the reap after
\     the capture, which has to hear it too. Asserted: check.f ends inside
\     EXIT-MS, in a case for each wait.
\  6. A STOP THAT FINDS NO CHILD. check.f answers a stop after its run however
\     the run ended, and a refused program, a usage error or a list of resident
\     inputs ends before any child is started. Its capture row then names pid 0,
\     and a kill of pid 0 is a kill of check.f's own process group. Asserted:
\     check.f fed a refused program on stdin after the signal dies of that
\     signal and names no step of its answer that threw.
\  7. A RUN PAST ITS DEADLINE. The capture that waits on check.f's child kills
\     it at the deadline and throws E-PROC-TIMEOUT, which left check.f as an
\     uncaught throw - exit 67 and a bare code - and its kill ended the child
\     alone: the sleeper went on under init. Asserted: check.f exits 70, its
\     refusal, with one line naming the subject and the deadline, in its
\     default mode with the run's output open and in --json-errors with it
\     closed, so the deadline passes in each wait; and failures 1 to 3 do not
\     happen.
\
\ A FAILED CASE LEAVES NOTHING EITHER. Each case ends through CASE-END whatever
\ it asserted or threw: a check.f still running is killed and reaped, and the
\ case waits for the processes it could not end, which the sleeper's 20 s
\ bounds. The scratch is under this process's runner root, which its registered
\ cleanup removes.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/fs.f
require lib/fs-list.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/signal.f
require lib/engine-candidate.f
require lib/test/runner.f
require src/core/include.f               \ SOURCE-ROOT:CANON-OS, the name --json-errors gives a file

package CHECK-SIGNAL-TEST

60000 constant READY-MS              \ check.f's static pass and two engine boots under a loaded pool
10000 constant EXIT-MS               \ half the sleeper's 20 s: a check.f that waited it out is late
5000 constant GONE-MS                \ the killed take a moment to finish dying
5000 constant DEADLINE-MS            \ the subject reported 0.6 s after check.f began, at load 15; the sleeper outlasts it
30000 constant EXPIRE-MS             \ past the sleeper's 20 s, for a case that failed to end it
1 cells constant REPORT-BYTES        \ the subject's pid
512 constant FEED-CHUNK              \ PIPE_BUF on macOS: a write poll admitted cannot block
$20000 constant PAD-BYTES            \ twice the 64 KiB a pipe holds on either host
64 constant SINK-CAP
$8000 constant LOG-CAP
-1 constant NO-FD

create REPORT REPORT-BYTES allot
create SINK SINK-CAP allot
create LOG-TEXT LOG-CAP allot
create TMP FS-PATH-CAP allot
create LOG FS-PATH-CAP allot
create FEED FEED-CHUNK allot         \ newlines: blank lines to check.f

variable REPORT-U                    \ bytes of REPORT read so far
variable PIPE-RD
variable PIPE-WR
variable CHECK-PID
variable CHECK-WATCH
variable FEED-WR                     \ the write end of check.f's stdin
variable TMP-U
variable LOG-U
variable ENTRY-N

TYPED-VARIABLE CHECK-LIVE bool       \ started and not yet reaped
TYPED-VARIABLE PIPE-DONE bool        \ the read end has reported end of file
TYPED-VARIABLE CASE-CLOSED bool      \ the subject closes its output before it reports
false CHECK-LIVE !
true PIPE-DONE !
false CASE-CLOSED !
NO-FD PIPE-RD !
NO-FD PIPE-WR !
NO-FD CHECK-WATCH !
NO-FD FEED-WR !

: TMP$ ( -- ptr u8 n )    TMP TMP-U @ ;
: LOG$ ( -- ptr u8 n )    LOG LOG-U @ ;

: CLOSE-CELL ( ptr n -- ) {: p:ptr :}
   p @ 0 >= if p @ close then
   NO-FD p ! ;

\ A process is started with its caller's ignored signals still ignored, and a
\ check.f that inherited one would never see it. RELEASE restores the default
\ action for every signal CATCH installed (docs/signal.md).
: SIGNALS-DEFAULT ( -- )
   SIGNAL:INIT
   SIGNAL:SIGTERM SIGNAL:CATCH
   SIGNAL:SIGINT SIGNAL:CATCH
   SIGNAL:SIGHUP SIGNAL:CATCH
   SIGNAL:RELEASE ;

\ ---- the scratch ------------------------------------------------------------

\ TMP, under this test's runner root, is the HB_TMP check.f is handed, so what
\ check.f leaves is whatever TMP holds; its output sits beside it.
: PATHS! ( -- )
   GT-ROOT s" tmp" TMP JOIN-PATH TMP-U !
   GT-ROOT s" check.log" LOG JOIN-PATH LOG-U !
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

\ The write end stays inheritable: it is what every process under check.f
\ holds. The read end is this process's alone.
: OPEN-PIPE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   r FD>N PIPE-RD !
   w FD>N PIPE-WR !
   0 REPORT-U !
   false PIPE-DONE ! ;

: FD-TEXT$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-INT SB$ ;

\ TRUE once fd answers one of the poll events, within ms.
: POLL-WITHIN? ( n n n -- bool ) {: fd:n events:n ms:n :}
   fd >FD events PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

\ TRUE once fd has bytes to read or has reached end of file, within ms.
: READY-WITHIN? ( n n -- bool )
   POLLIN swap POLL-WITHIN? ;

\ The subject's pid, before READY-MS runs out. End of file first means check.f
\ or its child died before the subject ran.
: REPORT-READ? ( -- bool )
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   begin REPORT-U @ REPORT-BYTES < while
      PIPE-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ REPORT REPORT-U @ + REPORT-BYTES REPORT-U @ - read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file within ms: nothing under check.f holds the write end.
: GONE-WITHIN? ( n -- bool ) {: ms:n :}
   PIPE-DONE @ if true exit then
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      PIPE-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ SINK SINK-CAP read {: got:n :}
      got 0= if true PIPE-DONE ! true exit then
      got 0 < if false exit then
   again ;

\ ---- check.f ----------------------------------------------------------------

: OPEN-NULL ( -- n )
   s\" /dev/null\z" drop open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: OPEN-LOG ( -- n )
   LOG$ FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

\ The report pipe, and the arguments check.f starts with in every case.
: CHECK-BEGIN ( -- )
   OPEN-PIPE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+ ;

\ check.f, with the case's arguments and environment added and in as its stdin.
\ The watch is opened while check.f is alive: macOS cannot register a process
\ that has already exited (test/proc-watch-smoke.f).
: CHECK-SPAWN ( n -- ) {: in:n :}
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   OPEN-LOG {: log:n :}
   ENGINE-CANDIDATE:PATH$ >LEN in >FD log >FD log >FD PROC-SPAWN-ARGV-ENV-IO
   PID>N CHECK-PID !
   true CHECK-LIVE !
   in close
   log close
   PIPE-WR CLOSE-CELL
   CHECK-PID @ proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then CHECK-WATCH ! ;

: SUBJECT$ ( -- ptr u8 n )
   s" test/check-signal-subject.f" ;

\ The subject, after whatever options check.f is given ahead of it.
: SUBJECT-START ( -- )
   SUBJECT$ >LEN PROC-ARGV+
   s" CHECK_SIGNAL_FD" >LEN PIPE-WR @ FD-TEXT$ >LEN PROC-ENV+
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
   w PROC-NOSIGPIPE!
   w FD>N FEED-WR !
   r FD>N CHECK-SPAWN ;

\ u bytes at a into check.f's stdin once the pipe has room for them, before
\ the deadline. u is at most FEED-CHUNK, so the write cannot block.
: FEED? ( ptr u8 n n -- bool ) {: a:ptr u:n deadline:n :}
   FEED-WR @ POLLOUT deadline PROC-LEFT-MS MS>N POLL-WITHIN? 0= if false exit then
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
   FEED-WR CLOSE-CELL ;

: CHECK-SIGNAL ( n -- ) {: sig:n :}
   CHECK-PID @ >PID sig PROC-KILL-RAW drop ;

: CHECK-ENDED-WITHIN? ( n -- bool ) {: ms:n :}
   CHECK-WATCH @ ms READY-WITHIN? ;

: CHECK-ENDED? ( -- bool )
   EXIT-MS CHECK-ENDED-WITHIN? ;

: CHECK-REAP ( -- outcome )
   CHECK-PID @ >PID PROC-WAIT-OUTCOME
   false CHECK-LIVE ! ;

: KILLED-BY? ( outcome n -- bool ) {: sig:n :}
   MATCH outcome
      exited OF drop false ENDOF
      signaled OF sig = ENDOF
      timeout OF false ENDOF
   ;MATCH ;

\ A check.f still running is ended here. What it printed says why it was late;
\ the kill's own outcome is no claim of this test.
: CHECK-END ( -- )
   CHECK-LIVE @ 0= if exit then
   CHECK-PID @ >PID SIGKILL PROC-KILL-RAW drop
   CHECK-REAP PROC-OUTCOME>RC drop
   s" the late check.f's output:" type cr
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP READ-ALL type cr ;

\ check.f names each step of its answer that threw (tools/check-core.f
\ CHK-SAY-THROW).
: LOG-THREW? ( -- bool )
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP READ-ALL s" threw" CONTAINS? ;

: CASE-BODY ( n -- ) {: sig:n :}
   CHECK-START
   s" signal: the subject came up under check.f" T-LABEL
   REPORT-READ? TTRUE
   sig CHECK-SIGNAL
   s" signal: check.f ended inside its bound" T-LABEL
   CHECK-ENDED? dup TTRUE
   if
      s" signal: check.f's status names the signal" T-LABEL
      CHECK-REAP sig KILLED-BY? TTRUE
      s" signal: the scratch is gone when check.f has ended" T-LABEL
      TMP-ENTRIES 0 T=
   then
   CHECK-END
   s" signal: no process under check.f is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE ;

\ SIGTERM lands while check.f reads its program, so it answers the stop once
\ the program is refused, and no child was ever started.
: STDIN-BODY ( -- )
   CHECK-START-STDIN
   s" stdin: check.f read past what the pipe holds" T-LABEL
   FEED-PAD? TTRUE
   SIGNAL:SIGTERM CHECK-SIGNAL
   s" stdin: check.f took the refused program" T-LABEL
   FEED-REFUSED? TTRUE
   s" stdin: check.f ended inside its bound" T-LABEL
   CHECK-ENDED? dup TTRUE
   if
      s" stdin: check.f's status names the signal" T-LABEL
      CHECK-REAP SIGNAL:SIGTERM KILLED-BY? TTRUE
      s" stdin: no step of the answer threw" T-LABEL
      LOG-THREW? TFALSE
      s" stdin: the scratch is gone when check.f has ended" T-LABEL
      TMP-ENTRIES 0 T=
   then
   CHECK-END
   s" stdin: no process under check.f is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE ;

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

\ check.f counts the deadline from the run's start, which is before the report,
\ so after the report it ends inside the deadline and the EXIT-MS a signal's
\ answer gets.
: LATE-BODY ( bool -- ) {: json:bool :}
   json LATE-START
   s" deadline: the subject came up under check.f" T-LABEL
   REPORT-READ? TTRUE
   s" deadline: check.f ended inside its bound" T-LABEL
   DEADLINE-MS EXIT-MS + CHECK-ENDED-WITHIN? dup TTRUE
   if
      s" deadline: check.f exits its refusal status" T-LABEL
      CHECK-REAP PROC-OUTCOME>RC RC>N 70 T=
      s" deadline: one line names the subject and the deadline" T-LABEL
      LOG-TEXT LOG$ LOG-TEXT LOG-CAP READ-ALL json LATE-LINE$ T$=
      s" deadline: the scratch is gone when check.f has ended" T-LABEL
      TMP-ENTRIES 0 T=
   then
   CHECK-END
   s" deadline: no process under check.f is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE ;

\ Whatever the body asserted or threw. Every step is a no-op for a case that
\ passed.
: CASE-END ( -- )
   CHECK-END
   PIPE-DONE @ 0= if
      s" the processes check.f left ended on their own" T-LABEL
      EXPIRE-MS GONE-WITHIN? TTRUE
   then
   CHECK-WATCH CLOSE-CELL
   FEED-WR CLOSE-CELL
   PIPE-WR CLOSE-CELL
   PIPE-RD CLOSE-CELL
   TMP-CLEAR ;

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

\ check-signal-test.f - a stopped check.f, and a check run past its deadline,
\ leave no process and no scratch behind; a verifier or a run that writes past
\ its capture, and a path the run cannot quote, each end check.f with one record.
\ Runs tools/check.f on test/check-signal-subject.f as a child with a private
\ TMPDIR; check.f's run stage runs the subject in a child of its own, which
\ starts a sleeper and reports. Each case signals check.f once the report is in,
\ or gives it a deadline the subject's wait on its sleeper outlasts.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/signal.f
require src/lib/engine-candidate.f
require src/lib/test/runner.f
require src/lib/test/fifo.f
require src/core/include.f               \ SOURCE-ROOT:CANON-OS, the name --json-errors gives a file
require src/lib/fd.f

package CHECK-SIGNAL-TEST

10000 constant EXIT-MS               \ half the sleeper's 20 s: a check.f that waited it out is late
5000 constant DEADLINE-MS            \ the subject reported 0.6 s after check.f began, at load 15; the sleeper outlasts it
1 cells constant REPORT-BYTES        \ the subject's pid
512 constant FEED-CHUNK              \ PIPE_BUF on macOS: a write poll admitted cannot block
$20000 constant PAD-BYTES            \ twice the 64 KiB a pipe holds on either host
$8000 constant LOG-CAP
$40000 constant VERIFY-ERR-CAP       \ the bytes of stderr check.f's verifier capture holds
$8000 constant RUN-OUT-CAP           \ the bytes of stdout check.f's run capture holds

create REPORT REPORT-BYTES allot
create LOG-TEXT LOG-CAP allot
create TMP PATH-CAP allot
create LOG PATH-CAP allot
create OUT PATH-CAP allot
create STANDIN PATH-CAP allot
create QUOTED PATH-CAP allot
create LOUD PATH-CAP allot
create BAD-TMP PATH-CAP allot
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
variable OUT-U
variable STANDIN-U
variable QUOTED-U
variable LOUD-U
variable BAD-TMP-U
variable ENTRY-N

TYPED-VARIABLE CHECK-LIVE bool       \ started and not yet reaped
TYPED-VARIABLE FIFO-DONE bool        \ the read end has reported end of file
TYPED-VARIABLE CASE-CLOSED bool      \ the subject closes its output before it reports
false CHECK-LIVE !
true FIFO-DONE !
false CASE-CLOSED !
PROCESS:NO-FD FIFO-RD !
PROCESS:NO-FD CHECK-WATCH !
PROCESS:NO-FD FEED-WR !

: TMP$ ( -- str )    TMP TMP-U @ >str ;
: LOG$ ( -- str )    LOG LOG-U @ >str ;
: OUT$ ( -- str )    OUT OUT-U @ >str ;
: STANDIN$ ( -- str )   STANDIN STANDIN-U @ >str ;
: QUOTED$ ( -- str )   QUOTED QUOTED-U @ >str ;
: LOUD$ ( -- str )   LOUD LOUD-U @ >str ;
: BAD-TMP$ ( -- str )   BAD-TMP BAD-TMP-U @ >str ;
: FIFO$ ( -- str )   FIFO FIFO-U @ >str ;

\ A process is started with its caller's ignored signals still ignored, and a
\ check.f that inherited one would never see it. RELEASE restores the default
\ action for every signal CATCH installed.
: SIGNALS-DEFAULT ( -- )
   SIGNAL:INIT
   SIGNAL:SIGTERM SIGNAL:CATCH
   SIGNAL:SIGINT SIGNAL:CATCH
   SIGNAL:SIGHUP SIGNAL:CATCH
   SIGNAL:RELEASE {: last:n :}
   last 0<> if getpid >PID last PROCESS:KILL-RAW drop then ;

\ TMP, under this test's runner root, is the TMPDIR check.f is handed, so what
\ check.f leaves is whatever TMP holds; its stderr and stdout sit beside it.
: PATHS! ( -- )
   GT-ROOT s" tmp" TMP FS:JOIN-PATH TMP-U !
   GT-ROOT s" check.log" LOG FS:JOIN-PATH LOG-U !
   GT-ROOT s" check.out" OUT FS:JOIN-PATH OUT-U !
   GT-ROOT s" standin" STANDIN FS:JOIN-PATH STANDIN-U !
   GT-ROOT s\" q\nx.f" QUOTED FS:JOIN-PATH QUOTED-U !
   GT-ROOT s" loud.f" LOUD FS:JOIN-PATH LOUD-U !
   GT-ROOT s\" t\"d" BAD-TMP FS:JOIN-PATH BAD-TMP-U !
   TMP$ FS:MAKE-DIRS ;

: ENTRY+ ( str -- )
   drop ENTRY-N @ 1+ ENTRY-N ! ;

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

: FD-TEXT$ ( n -- str )
   SB-RESET FMT:SB-INT SB$ ;

\ fd within ms: a positive count once it answers one of the poll events, 0 when
\ ms ran out first, a negative errno when the poll failed; a negative ms has no
\ limit.
: POLL-WITHIN ( n n n -- n )
   {: fd:n events:n ms:n :}
   fd >FD events PROCESS:PFD!
   1 ms ms >MS PROCESS:DEADLINE-AT PROCESS:POLL-RESTART ;

\ fd within ms, answered as POLL-WITHIN answers: ready once it has bytes to read
\ or has reached end of file. A descriptor the poll reports broken is no answer
\ either way: it throws E-PROC-OUTPUT.
: READY-WITHIN ( n n -- n )
   FD:POLLIN swap POLL-WITHIN {: ready:n :}
   0 >IDX TEST-FIFO:BROKEN? if E-PROC-OUTPUT throw then
   ready ;

\ The subject's pid. check.f ending first, end of file, a short report or a
\ failed read answers false.
: REPORT-READ? ( -- bool )
   begin REPORT-U @ REPORT-BYTES < while
      FIFO-RD @ >FD CHECK-WATCH @ >FD TEST-FIFO:REPORT-OR-END
      0 >IDX PROCESS:PFD-REVENTS 0= if false exit then
      FIFO-RD @ REPORT REPORT-U @ + REPORT-BYTES REPORT-U @ - SPAN:MAKE read {: got:n :}
      got 0 <= if false exit then
      REPORT-U @ got + REPORT-U !
   repeat
   true ;

\ End of file: nothing under check.f holds the write end. A failed read or poll
\ answers false. A FIFO no writer has opened reads end of file too, so it means
\ anything only after the subject's report.
: GONE? ( -- bool )
   FIFO-DONE @ if true exit then
   FIFO-RD @ >FD -1 TEST-FIFO:EOF-WITHIN {: eof:n :}
   eof 0 <= if false exit then
   true FIFO-DONE !
   true ;

: OPEN-NULL ( -- n )
   s\" /dev/null\z" STR:DATA open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: OPEN-LOG ( str -- n )
   FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   fd ;

: CHECK-BEGIN ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" PROC-ARGV+
   s" tools/check.f" PROC-ARGV+ ;

\ check.f with stdin in, stderr into LOG and stdout into OUT. The watch opens
\ while check.f is alive: macOS cannot register a process that has exited.
: CHECK-SPAWN-TMP ( str n -- ) {: tmp:str in:n :}
   s" TMPDIR" tmp PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   LOG$ OPEN-LOG {: log:n :}
   OUT$ OPEN-LOG {: out:n :}
   ENGINE-CANDIDATE:PATH$ in >FD out >FD log >FD PROC-SPAWN-ARGV-ENV-IO
   PID>N CHECK-PID !
   true CHECK-LIVE !
   in close
   log close
   out close
   CHECK-PID @ proc-watch-open dup 0 < if drop E-PROC-OUTPUT throw then CHECK-WATCH ! ;

: CHECK-SPAWN ( n -- ) {: in:n :}
   TMP$ in CHECK-SPAWN-TMP ;

: SUBJECT$ ( -- str )
   s" test/check-signal-subject.f" ;

\ The subject and its report FIFO, after whatever options check.f is given
\ ahead of it.
: SUBJECT-START ( -- )
   OPEN-FIFO
   SUBJECT$ PROC-ARGV+
   s" CHECK_SIGNAL_FIFO" FIFO$ PROC-ENV+
   CASE-CLOSED @ if s" CHECK_SIGNAL_CLOSED" s" 1" PROC-ENV+ then
   OPEN-NULL CHECK-SPAWN ;

: CHECK-START ( -- )
   CHECK-BEGIN
   s" --" PROC-ARGV+
   SUBJECT-START ;

\ check.f with a deadline of DEADLINE-MS, in --json-errors mode or its default.
: LATE-START ( bool -- ) {: json:bool :}
   CHECK-BEGIN
   s" --" PROC-ARGV+
   s" --deadline-ms" PROC-ARGV+
   DEADLINE-MS FD-TEXT$ PROC-ARGV+
   json if s" --json-errors" PROC-ARGV+ then
   SUBJECT-START ;

\ check.f with no input named reads its program from stdin, a pipe this test
\ writes. The write end is this process's alone, or check.f would hold its own
\ stdin open.
: CHECK-START-STDIN ( -- )
   CHECK-BEGIN
   FD:PIPE-PAIR {: r:fd w:fd :}
   w FD:FD-CLOEXEC!
   w FD:FD-NOSIGPIPE!
   w FD>N FEED-WR !
   r FD>N CHECK-SPAWN ;

\ u bytes at a into check.f's stdin once the pipe has room for them. u is at
\ most FEED-CHUNK, so the write cannot block. A short write or a failed poll
\ answers false.
: FEED? ( str -- bool )
   {: s:str :}
   FEED-WR @ FD:POLLOUT -1 POLL-WITHIN {: ready:n :}
   ready 0 < if false exit then
   FEED-WR @ s write s STR:LEN = ;

: FEED-FILL ( -- )
   FEED-CHUNK 0 ?do 10 FEED i + c! loop ;

\ More blank lines than the pipe holds: the last of them goes in only once
\ check.f reads its program, which it does after it has caught the stops.
: FEED-PAD? ( -- bool )
   FEED-FILL
   PAD-BYTES FEED-CHUNK / 0 ?do
      FEED FEED-CHUNK >str FEED? 0= if false unloop exit then
   loop
   true ;

\ A definition that leaves nothing for its declared n, then end of file.
: FEED-REJECTED? ( -- bool )
   s" : BAD ( -- n ) ;" FEED?
   FEED-WR PROCESS:CLOSE-CELL ;

: CHECK-SIGNAL ( n -- ) {: sig:n :}
   CHECK-PID @ >PID sig PROCESS:KILL-RAW drop ;

\ Returns once check.f has ended, asserting it ended within ms; a failed poll
\ of the watch, or one that reports it broken, throws E-PROC-OUTPUT.
: CHECK-ENDED-WITHIN ( n -- )
   {: ms:n :}
   mono-ns {: began:n :}
   CHECK-WATCH @ -1 READY-WITHIN {: ready:n :}
   ready 0 < if E-PROC-OUTPUT throw then
   s" check.f ended within its bound" T-LABEL
   mono-ns began - ms PROCESS:NS-PER-MS * < TTRUE ;

\ Returns once check.f has ended, inside the EXIT-MS a signal's answer gets.
: CHECK-ENDED ( -- )
   EXIT-MS CHECK-ENDED-WITHIN ;

: CHECK-REAP ( -- PROCESS:status )
   CHECK-PID @ >PID PROCESS:WAIT
   false CHECK-LIVE ! ;

: KILLED-BY? ( PROCESS:status n -- bool ) {: sig:n :}
   MATCH PROCESS:status
      exited OF drop false ENDOF
      signaled OF sig = ENDOF
   ;MATCH ;

\ A check.f still running is ended here. What it printed says why it was late;
\ the kill's own outcome is no claim of this test.
: CHECK-END ( -- )
   CHECK-LIVE @ 0= if exit then
   CHECK-PID @ >PID PROCESS:SIGKILL PROCESS:KILL-RAW drop
   CHECK-REAP drop
   s" the late check.f's output:" type cr
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL >str type cr ;

\ check.f names each step of its answer that threw (tools/check-core.f
\ CHK-SAY-THROW), and src/lib/process.f a walk of the child's tree it could not
\ finish (PROCESS:WALK-REPORT).
: LOG-THREW? ( -- bool )
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL >str {: s:str :}
   s s" threw" STR:CONTAINS? s s" not walked" STR:CONTAINS? or ;

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
\   wait has a case (CASE-CLOSED), and each asserts check.f ends within EXIT-MS.
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
   GONE? TTRUE ;

\ check.f answers a stop after its run however the run ended, and a rejected
\ program ends before any child is started: its capture row names pid 0, and a
\ kill of pid 0 kills check.f's own process group. SIGTERM lands while check.f
\ reads its program, so it answers once the program is rejected. Nothing under
\ check.f could open a FIFO: this case makes none and asserts check.f's status.
: STDIN-BODY ( -- )
   CHECK-START-STDIN
   s" stdin: check.f read past what the pipe holds" T-LABEL
   FEED-PAD? TTRUE
   SIGNAL:SIGTERM CHECK-SIGNAL
   s" stdin: check.f took the rejected program" T-LABEL
   FEED-REJECTED? TTRUE
   CHECK-ENDED
   s" stdin: check.f's status names the signal" T-LABEL
   CHECK-REAP SIGNAL:SIGTERM KILLED-BY? TTRUE
   s" stdin: no step of the answer threw" T-LABEL
   LOG-THREW? TFALSE
   s" stdin: the scratch is gone when check.f has ended" T-LABEL
   TMP-ENTRIES 0 T= ;

\ The record check.f ends a run past its deadline with, in --json-errors mode or
\ its default. It names the subject by its canonical path, as each of its
\ reports does.
: LATE-RECORD$ ( bool -- str ) {: json:bool :}
   SB-RESET
   json if
      s\" {\"severity\":\"error\",\"code\":\"E-PROC-TIMEOUT\",\"message\":\"" SB-APPEND
   else
      s" error[E-PROC-TIMEOUT]: " SB-APPEND
   then
   SUBJECT$ SOURCE-ROOT:CANON-OS drop SB-APPEND
   s" : the run passed its deadline" SB-APPEND
   json if
      s\" \",\"notes\":{\"deadline\":\"" SB-APPEND
      DEADLINE-MS FMT:SB-INT
      s\"  ms\"},\"help\":\"check it with a longer `--deadline-ms`, up to 2147483647\"}\n" SB-APPEND
   else
      s\" \n  = deadline: " SB-APPEND
      DEADLINE-MS FMT:SB-INT
      s\"  ms\n  = help: check it with a longer `--deadline-ms`, up to 2147483647\n\n" SB-APPEND
   then
   SB$ ;

\ The capture that waits on check.f's child kills it at the deadline and throws
\ E-PROC-TIMEOUT. Uncaught, that is exit 85 and a bare code, and its kill ends
\ the child alone, leaving the sleeper under init. check.f must exit 70, its
\ rejection, with one record on stderr naming the subject and the deadline and
\ nothing on stdout, and leak nothing:
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
   s" deadline: check.f exits its rejection status" T-LABEL
   CHECK-REAP PROCESS:STATUS>RC RC>N 70 T=
   s" deadline: one record on stderr names the subject and the deadline" T-LABEL
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL >str json LATE-RECORD$ T$=
   s" deadline: stdout holds no record" T-LABEL
   OUT$ FS:FILE-SIZE 0 T=
   s" deadline: the scratch is gone when check.f has ended" T-LABEL
   TMP-ENTRIES 0 T=
   CHECK-END
   s" deadline: no process under check.f is left" T-LABEL
   GONE? TTRUE ;

\ A stand-in engine that, given the verifier child's file, writes more stderr
\ than the verifier's capture holds, with no newline, then runs as hb.
: STANDIN-WRITE ( -- )
   SB-RESET
   s\" #!/bin/sh\nfor a in \"$@\"; do case \"$a\" in *check-verify-child.f) head -c 300000 /dev/zero | tr '\\0' x >&2;; esac; done\nexec '" SB-APPEND
   ENGINE-CANDIDATE:PATH$ SB-APPEND
   s\" ' \"$@\"\n" SB-APPEND
   STANDIN$ SB$ FS:WRITE-ALL
   STANDIN$ FS:CHMOD-X ;

: TRUNCATED-RECORD$ ( -- str )
   SUBJECT$ SOURCE-ROOT:CANON-OS drop {: p:str :}
   SB-RESET
   s" error[E-PROC-STDERR-CAPACITY]: " SB-APPEND  p SB-APPEND
   s\" : the verifier wrote past its capture of standard error\n  = stderr: at most " SB-APPEND
   VERIFY-ERR-CAP FMT:SB-INT
   s\"  bytes\n  = help: an engine defect wrote it, its stderr is shown above; run the program with `bin/hb " SB-APPEND
   p SB-APPEND
   s\" `\n\n" SB-APPEND
   SB$ ;

\ The pre-pass's verifier child writes past its capture: check.f relays the
\ stderr it captured, ends its line, and closes with one record, exit 69.
: TRUNCATED-BODY ( -- )
   STANDIN-WRITE
   CHECK-BEGIN
   s" --" PROC-ARGV+
   SUBJECT$ PROC-ARGV+
   s" HABU_UNDER_TEST" STANDIN$ PROC-ENV+
   OPEN-NULL CHECK-SPAWN
   CHECK-ENDED
   s" truncated: check.f exits unavailable" T-LABEL
   CHECK-REAP PROCESS:STATUS>RC RC>N 69 T=
   TRUNCATED-RECORD$ {: r:str :}
   LOG$ FS:FILE-SIZE {: size:n :}
   s" truncated: stderr is the captured stderr, a newline and one record" T-LABEL
   size  VERIFY-ERR-CAP 1+ r STR:LEN +  T=
   s" truncated: the record ends stderr" T-LABEL
   LOG$ size r STR:LEN - LOG-TEXT r STR:LEN FS:READ-AT
   LOG-TEXT r STR:LEN >str r T$=
   s" truncated: the record starts its own line" T-LABEL
   LOG$ size r STR:LEN - 1- LOG-TEXT 1 FS:READ-AT
   LOG-TEXT c@ 10 T= ;

: LOUD-RECORD$ ( -- str )
   LOUD$ SOURCE-ROOT:CANON-OS drop {: p:str :}
   SB-RESET
   s" error[E-PROC-STDOUT-CAPACITY]: " SB-APPEND  p SB-APPEND
   s" : the run wrote past its capture" SB-APPEND
   s\" \n  = stdout: at most " SB-APPEND  RUN-OUT-CAP FMT:SB-INT
   s\"  bytes\n  = help: run the program with `bin/hb " SB-APPEND
   p SB-APPEND  s" ` to see its whole output" SB-APPEND
   s\" , or make it write less\n\n" SB-APPEND
   SB$ ;

\ A program that prints past the run's stdout capture: check.f ends with one
\ record naming that code, the overflow and the stdout bound.
: LOUD-BODY ( -- )
   LOUD$ s\" : LOUD ( -- ) 40000 0 do [char] x emit loop ;\nLOUD\n"
   FS:WRITE-ALL
   CHECK-BEGIN
   s" --" PROC-ARGV+
   LOUD$ PROC-ARGV+
   OPEN-NULL CHECK-SPAWN
   CHECK-ENDED
   s" loud: check.f exits its rejection status" T-LABEL
   CHECK-REAP PROCESS:STATUS>RC RC>N 70 T=
   s" loud: stderr is the stdout record alone" T-LABEL
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL >str
   LOUD-RECORD$ T$= ;

: QUOTED-RECORD$ ( -- str )
   QUOTED$ SOURCE-ROOT:CANON-OS drop {: p:str :}
   SB-RESET
   s" error[E-FS-PATH-UNSAFE]: the path `" SB-APPEND  p STR:DATA p STR:LEN 4 - >str SB-APPEND  s" \nx.f" SB-APPEND
   s\" ` holds a double quote, backslash, CR, LF or NUL, so the run cannot quote it\n  = found: a LF at byte " SB-APPEND
   p STR:LEN 4 - FMT:SB-INT
   s\" \n  = help: rename or move the file so its path holds none of them\n\n" SB-APPEND
   SB$ ;

\ A source whose path holds a LF, which the run's loader line would quote: its
\ record shows the path on one line, the LF escaped.
: QUOTED-BODY ( -- )
   QUOTED$ s\" : X ( -- ) ;\n" FS:WRITE-ALL
   CHECK-BEGIN
   s" --" PROC-ARGV+
   QUOTED$ PROC-ARGV+
   OPEN-NULL CHECK-SPAWN
   CHECK-ENDED
   s" quoted: check.f exits its usage status" T-LABEL
   CHECK-REAP PROCESS:STATUS>RC RC>N 64 T=
   s" quoted: one record shows the path on one line and names the byte" T-LABEL
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL >str QUOTED-RECORD$ T$= ;

\ check.f's temporary directory is under a TMPDIR whose path holds a double
\ quote, which the run cannot quote.
: BAD-TMP-BODY ( -- )
   BAD-TMP$ FS:MAKE-DIRS
   CHECK-BEGIN
   s" --" PROC-ARGV+
   SUBJECT$ PROC-ARGV+
   BAD-TMP$ OPEN-NULL CHECK-SPAWN-TMP
   CHECK-ENDED
   s" tmpdir: check.f exits its usage status" T-LABEL
   CHECK-REAP PROCESS:STATUS>RC RC>N 64 T=
   s" tmpdir: the record names the double quote and TMPDIR" T-LABEL
   LOG-TEXT LOG$ LOG-TEXT LOG-CAP FS:READ-ALL >str
   BAD-TMP$ SOURCE-ROOT:CANON-OS drop STR:LEN {: pu:n :}
   SB-RESET
   s\" \n  = found: a double quote at byte " SB-APPEND
   pu 2 - FMT:SB-INT
   s\" \n  = help: set TMPDIR to a directory whose path holds none of them\n\n" SB-APPEND
   SB$ STR:ENDS-WITH? TTRUE ;

: LEFT-ENDED ( -- )
   FIFO-DONE @ 0= if
      s" the processes check.f left ended on their own" T-LABEL
      GONE? TTRUE
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
   [: CHECK-WATCH PROCESS:CLOSE-CELL
      FEED-WR PROCESS:CLOSE-CELL
      FIFO-RD PROCESS:CLOSE-CELL
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
   s" check-signal: SIGTERM, the child's output closed" type cr
   SIGNAL:SIGTERM true SIGNAL-CASE
   s" check-signal: SIGTERM before any child, a rejected program on stdin" type cr
   [: STDIN-BODY ;] [: CASE-END ;] finally
   s" check-signal: a run past its deadline" type cr
   false false LATE-CASE
   s" check-signal: a run past its deadline, --json-errors, the child's output closed" type cr
   true true LATE-CASE
   s" check-signal: a verifier that writes past its capture" type cr
   [: TRUNCATED-BODY ;] [: CASE-END ;] finally
   s" check-signal: a run that writes past its stdout capture" type cr
   [: LOUD-BODY ;] [: CASE-END ;] finally
   s" check-signal: a source path holding a LF" type cr
   [: QUOTED-BODY ;] [: CASE-END ;] finally
   s" check-signal: a TMPDIR holding a double quote" type cr
   [: BAD-TMP-BODY ;] [: CASE-END ;] finally
   GT-CLEANUP ;

;package

CHECK-SIGNAL-TEST:MAIN
T-REPORT
s" check-signal-test: ok" type cr

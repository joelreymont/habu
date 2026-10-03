\ runner.f - checked native test runner foundation.

require lib/errors.f
require lib/string.f
require lib/span.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test/record.f
require lib/fmt.f                        \ FMT:.INT - one-line number text, shared builder untouched

2 constant GT-EX-FAIL
256 constant GT-FAIL-MAX
128 constant GT-FAIL-NAME-CAP
32768 constant GT-OUT-CAP
32768 constant GT-ERR-CAP
10000 constant GT-DEFAULT-TIMEOUT-MS
5000 constant GT-HEARTBEAT-MS
10 constant GT-LF

create GT-ROOT-BUF FS-PATH-CAP allot
create GT-OUT-BUF GT-OUT-CAP allot
create GT-ERR-BUF GT-ERR-CAP allot
create GT-FAIL-NAMES GT-FAIL-MAX GT-FAIL-NAME-CAP * allot
create GT-FAIL-US GT-FAIL-MAX cells allot

variable GT-ROOT-U
variable GT-OUT-U
variable GT-ERR-U
variable GT-EXITED                       \ bool: completed by exit (vs signal) when not timed out
variable GT-TIMED-OUT                    \ bool: capture deadline hit
variable GT-CODE                         \ exit code or signal number; 0 for timeout
variable GT-FAIL#
variable GT-PROGRESS-START-NS
variable GT-PROGRESS-LAST-NS
variable GT-LINE-END
variable GT-FLUSH-U
variable GT-TAIL-U

: GT-FAIL-SLOT ( n -- ptr u8 ) {: idx :}
   idx 0 < if E-TBL-BOUNDS throw then
   idx GT-FAIL-MAX >= if E-TBL-BOUNDS throw then
   idx GT-FAIL-NAME-CAP * GT-FAIL-NAMES + ;

: GT-FAIL-U-PTR ( n -- ptr n ) {: idx :}
   idx 0 < if E-TBL-BOUNDS throw then
   idx GT-FAIL-MAX >= if E-TBL-BOUNDS throw then
   idx cells GT-FAIL-US + ;

: GT-FAIL-NAME$ ( n -- ptr u8 n ) {: idx :}
   idx GT-FAIL-SLOT
   idx GT-FAIL-U-PTR @ ;

: GT-RESET ( -- )
   0 GT-OUT-U !
   0 GT-ERR-U !
   0 0= GT-EXITED !
   0 0= 0= GT-TIMED-OUT !
   0 GT-CODE !
   0 GT-FAIL# ! ;

: GT-ROOT ( -- ptr u8 n )
   GT-ROOT-BUF GT-ROOT-U @ ;

: GT-OUT$ ( -- ptr u8 n )
   GT-OUT-BUF GT-OUT-U @ ;

: GT-ERR$ ( -- ptr u8 n )
   GT-ERR-BUF GT-ERR-U @ ;

: GT-FAILURES ( -- n )
   GT-FAIL# @ ;

: GT-EXPECT-ROOT ( -- )
   GT-ROOT-U @ 0 <= if E-FS-PATH throw then ;

: GT-COPY-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GT-ROOT-BUF u BYTE-COPY
   u GT-ROOT-U ! ;

: GT-START ( ptr u8 n -- ) {: prefix:ptr prefixu :}
   GT-RESET
   CLEANUP-RESET
   0 GT-ROOT-U !
   prefix prefixu HB-TMP-MKDIR GT-COPY-ROOT!
   GT-ROOT CLEANUP-TREE+ ;

: GT-CLEANUP ( -- )
   CLEANUP-RUN ;

: GT-PATH ( ptr u8 n ptr u8 -- n ) {: name:ptr nameu dst:ptr :}
   GT-EXPECT-ROOT
   GT-ROOT name nameu dst JOIN-PATH ;

: GT-FAIL-STORED ( -- n )
   GT-FAIL# @ GT-FAIL-MAX > if GT-FAIL-MAX exit then
   GT-FAIL# @ ;

: GT-FAIL-NAME! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   GT-FAIL# @ GT-FAIL-MAX >= if exit then
   name GT-FAIL# @ GT-FAIL-SLOT nameu BYTE-COPY
   nameu GT-FAIL# @ GT-FAIL-U-PTR ! ;

: GT-FAIL+ ( ptr u8 n -- ) {: name:ptr nameu:n :}
   nameu 0 < if E-TBL-FIELD throw then
   nameu GT-FAIL-NAME-CAP > if E-TBL-FIELD throw then
   name nameu GT-FAIL-NAME!
   GT-FAIL# @ 1+ GT-FAIL# !
   s" runner" GT-FAIL# @ name nameu TREC-FAIL ;

: GT-CHECK ( bool ptr u8 n -- ) {: ok name:ptr nameu :}
   ok 0= if name nameu GT-FAIL+ then ;

\ A comparison keeps both sides: reducing them to a bool first leaves the
\ reader with a label and no evidence. Detail lines follow the TFAIL record in
\ the shape lib/test/assert.f prints for T= and T$=, under this layer's name:
\ one line per numeric comparison, so FMT:.INT and not `.`, whose text ends in
\ a newline of its own.
: GT-CHECK-N ( n n ptr u8 n -- ) {: got:n want:n name:ptr nameu:n :}
   got want <> if
      name nameu GT-FAIL+
      s" runner: expected " type want FMT:.INT
      s"  got " type got FMT:.INT
      cr
   then ;

: GT-CHECK$ ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: got:ptr gotu:n want:ptr wantu:n name:ptr nameu:n :}
   got gotu want wantu STR= 0= if
      name nameu GT-FAIL+
      s" runner: expected string:" type cr
      want wantu type cr
      s" got string:" type cr
      got gotu type cr
   then ;

\ Decompose an outcome into the runner's exited/timed-out/code cells (all one
\ cell, lossless: exit codes >= 128 stay distinct from signal deaths).
: GT-OUTCOME! ( outcome -- )
   MATCH outcome
     exited OF GT-CODE ! 0 0= GT-EXITED ! 0 0= 0= GT-TIMED-OUT ! ENDOF
     signaled OF GT-CODE ! 0 0= 0= GT-EXITED ! 0 0= 0= GT-TIMED-OUT ! ENDOF
     timeout OF 0 GT-CODE ! 0 0= 0= GT-EXITED ! 0 0= GT-TIMED-OUT ! ENDOF
   ;MATCH ;

: GT-OUTCOME@ ( -- outcome )
   GT-TIMED-OUT @ if OUTCOME:TIMEOUT exit then
   GT-EXITED @ if GT-CODE @ OUTCOME:EXITED exit then
   GT-CODE @ OUTCOME:SIGNALED ;

: GT-STORE-RUN ( len len outcome -- )
   GT-OUTCOME!
   LEN>N GT-ERR-U !
   LEN>N GT-OUT-U ! ;

: GT-CAPTURE-STORE ( -- )
   PROC-CAPTURE-OUTCOME@ GT-STORE-RUN ;

: GT-RUN ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   path pathu >LEN GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN timeout >MS
   RUN-ARGV-CAPTURE-OUTCOME
   GT-STORE-RUN ;

: GT-CAPTURE-DRAIN ( -- )
   GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN PROC-DRAIN-READY ;

: GT-RUN-DEFAULT ( ptr u8 n -- )
   GT-DEFAULT-TIMEOUT-MS GT-RUN ;

\ ---- an in-process action's output -------------------------------------------
\ GT-CAPTURE-ACTION runs an action in this process with its stdout and stderr
\ pointed at two files under the caller's directory, then reads them back into
\ the caller's spans; code is the action's throw code, 0 when it returned.
\ Files, not pipes: nothing drains a pipe while the action runs, so output past
\ the pipe's buffer would block the action for good, and the action may run
\ captures of its own through the task's one-capture row (lib/process.f).
\ A file is unlinked once it is open twice, appending as the stream and reading
\ from its start for the capture, so no exit path leaves it behind and a
\ capture nested in the action opens its own under the same name. A capture's
\ state is a frame of its own: per stream, the stream it saved and the file's
\ reader. A nested capture takes the next frame, so each reads its own bytes
\ and the outer one gets its streams back; the streams are the process's, so
\ two tasks never capture at once. Every descriptor a frame holds sits at
\ GT-FD-HOLD-MIN or above and closes on exec, so a child the action spawns
\ inherits none. The read is its own bound: output past a span throws
\ E-PROC-TRUNCATED, as a child's capture does, even when a child the action
\ left running wrote it after the action returned. Both streams come back on
\ every exit path.
10 constant GT-FD-HOLD-MIN               \ a held descriptor sits above the low ones
1 constant GT-STDOUT-FD
2 constant GT-STDERR-FD
2 constant GT-CAP-ROWS                   \ a frame's rows: stdout, then stderr
\ Captures open at once: tests nest two, and one past the bound is refused
\ with E-TBL-BOUNDS before it takes a frame.
8 constant GT-CAP-NEST-MAX

\ Per frame and row, the stream the row saved and the row's file reader, -1
\ until the row holds one.
GT-CAP-NEST-MAX GT-CAP-ROWS * TYPED-BUFFER GT-CAP-SAVES n
GT-CAP-NEST-MAX GT-CAP-ROWS * TYPED-BUFFER GT-CAP-READERS n
variable GT-CAP-DEPTH                    \ frames in use
create GT-CAP-PATH FS-PATH-CAP allot     \ a file's path while GT-CAP-FILE opens it
1 BUFFER: GT-CAP-PROBE                   \ the byte a reader past a full span looks for

\ fd's row in the innermost frame.
: GT-CAP-ROW ( n -- n ) {: fd:n :}
   GT-CAP-DEPTH @ 1- GT-CAP-ROWS * fd GT-STDOUT-FD - + ;

: GT-CAP-SAVE ( n -- ptr n )
   GT-CAP-ROW GT-CAP-SAVES ;

: GT-CAP-READER ( n -- ptr n )
   GT-CAP-ROW GT-CAP-READERS ;

: GT-CAP-ENTER ( -- )
   GT-CAP-DEPTH @ GT-CAP-NEST-MAX >= if E-TBL-BOUNDS throw then
   GT-CAP-DEPTH @ 1+ GT-CAP-DEPTH !
   -1 GT-STDOUT-FD GT-CAP-SAVE !
   -1 GT-STDERR-FD GT-CAP-SAVE !
   -1 GT-STDOUT-FD GT-CAP-READER !
   -1 GT-STDERR-FD GT-CAP-READER ! ;

\ A copy of fd at GT-FD-HOLD-MIN or above, close-on-exec from its first
\ instant; -1 when the host refuses.
: GT-CAP-DUP ( n -- n )
   F-DUPFD-CLOEXEC GT-FD-HOLD-MIN fcntl ;

\ A new empty file named name under dir, open for appending (the writer) and,
\ held, for reading from its start (the reader). It is unlinked before either
\ comes back; on a throw nothing is held.
: GT-CAP-FILE ( ptr u8 n ptr u8 n -- n n )
   {: dir:ptr diru:n name:ptr nameu:n :}
   dir diru name nameu GT-CAP-PATH JOIN-PATH {: pathu:n :}
   GT-CAP-PATH pathu FS-PATHZ
   FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-O-APPEND or FS-MODE-0644 open {: wr:n :}
   wr 0 < if E-FS-OPEN throw then
   GT-CAP-PATH pathu FS-PATHZ open-rd {: opened:n :}
   GT-CAP-PATH pathu FS-PATHZ unlink {: gone:n :}
   opened 0 < if wr close E-FS-OPEN throw then
   opened GT-CAP-DUP {: rd:n :}
   opened close
   rd 0 < if wr close E-PROC-OUTPUT throw then
   gone 0 < if wr close rd close E-FS-IO throw then
   wr rd ;

\ fd writes to a new file named name under dir; fd's row keeps the stream fd
\ held before and the file's reader.
: GT-CAP-REDIRECT ( ptr u8 n ptr u8 n n -- )
   {: dir:ptr diru:n name:ptr nameu:n fd:n :}
   fd GT-CAP-DUP {: kept:n :}
   kept 0 < if E-PROC-OUTPUT throw then
   kept fd GT-CAP-SAVE !
   dir diru name nameu GT-CAP-FILE {: wr:n rd:n :}
   rd fd GT-CAP-READER !
   wr fd dup2 {: moved:n :}
   wr close-rc {: closed:n :}
   moved 0 < if E-PROC-OUTPUT throw then
   closed 0 < if E-PROC-OUTPUT throw then ;

\ One read from fd into dst past its first got bytes; 0 at the file's end.
: GT-CAP-READ-AT ( n n SPAN:span<u8> -- n )
   {: got:n fd:n dst :}
   fd dst got SPAN:SKIP SPAN:$ read {: rd:n :}
   rd 0 < if E-FS-IO throw then
   rd ;

\ The file behind the reader fd into dst, from its start. A byte past a full
\ dst throws E-PROC-TRUNCATED, whenever it was written.
: GT-CAP-READ ( n SPAN:span<u8> -- len )
   {: fd:n dst :}
   0 begin dup dst SPAN:LEN < while
      dup fd dst GT-CAP-READ-AT
      dup 0= if drop >LEN exit then
      +
   repeat
   fd GT-CAP-PROBE 1 read
   dup 0 < if E-FS-IO throw then
   0 > if E-PROC-TRUNCATED throw then
   >LEN ;

\ 0, or E-PROC-OUTPUT when fd, held unless it is -1, does not close.
: GT-CAP-CLOSE ( n -- n )
   {: fd:n :}
   fd 0 < if 0 exit then
   fd close-rc 0 < if E-PROC-OUTPUT exit then
   0 ;

\ fd takes back the stream kept holds, unless it is -1: 0, or E-PROC-OUTPUT.
: GT-CAP-RESTORE ( n n -- n )
   {: fd:n kept:n :}
   kept 0 < if 0 exit then
   kept fd dup2 0 < if E-PROC-OUTPUT exit then
   0 ;

\ The first of two codes that is not 0.
: GT-CAP-FIRST ( n n -- n )
   {: a:n b:n :}
   a 0<> if a exit then
   b ;

\ fd takes back the stream its row saved, and the row's descriptors close:
\ 0, or E-PROC-OUTPUT when any of that is refused.
: GT-CAP-RELEASE ( n -- n )
   {: fd:n :}
   fd fd GT-CAP-SAVE @ GT-CAP-RESTORE
   fd GT-CAP-SAVE @ GT-CAP-CLOSE GT-CAP-FIRST
   fd GT-CAP-READER @ GT-CAP-CLOSE GT-CAP-FIRST ;

\ Both rows are released whatever either refuses, then the frame goes and the
\ first refusal is thrown.
: GT-CAP-LEAVE ( -- )
   GT-STDOUT-FD GT-CAP-RELEASE
   GT-STDERR-FD GT-CAP-RELEASE GT-CAP-FIRST {: rc:n :}
   GT-CAP-DEPTH @ 1- GT-CAP-DEPTH !
   rc 0<> if rc throw then ;

\ The files are read with the action's streams still on them; GT-CAP-LEAVE
\ gives the streams back after the read, on its path and on every throw.
: GT-CAP-RUN ( [ -- ] ptr u8 n SPAN:span<u8> SPAN:span<u8> -- len len n )
   {: q dir:ptr diru:n out err :}
   dir diru s" capture-out.txt" GT-STDOUT-FD GT-CAP-REDIRECT
   dir diru s" capture-err.txt" GT-STDERR-FD GT-CAP-REDIRECT
   q catch {: code:n :}
   GT-STDOUT-FD GT-CAP-READER @ out GT-CAP-READ
   GT-STDERR-FD GT-CAP-READER @ err GT-CAP-READ
   code ;

: GT-CAPTURE-ACTION ( [ -- ] ptr u8 n SPAN:span<u8> SPAN:span<u8> -- len len n )
   {: q dir:ptr diru:n out err :}
   diru 0 <= if E-FS-PATH throw then
   GT-CAP-ENTER
   q dir diru out err [: GT-CAP-RUN ;] [: GT-CAP-LEAVE ;] finally ;

: GT-LINE-FLUSH-U ( ptr u8 n -- n ) {: a:ptr u :}
   u 0 < if E-STR-BOUNDS throw then
   0 GT-LINE-END !
   0 begin dup u < while
      dup a + c@ GT-LF = if dup 1+ GT-LINE-END ! then
      1+
   repeat drop
   GT-LINE-END @ ;

: GT-WRITE-FD ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0 < if E-STR-BOUNDS throw then
   u 0= if exit then
   fd a u write u <> if E-FS-IO throw then ;

: GT-FLUSH-LINES-FD ( n ptr u8 ptr n -- ) {: fd buf:ptr lenp:ptr :}
   lenp @ 0 < if E-STR-BOUNDS throw then
   buf lenp @ GT-LINE-FLUSH-U GT-FLUSH-U !
   GT-FLUSH-U @ 0 <= if exit then
   fd buf GT-FLUSH-U @ GT-WRITE-FD
   lenp @ GT-FLUSH-U @ - GT-TAIL-U !
   GT-TAIL-U @ 0 > if
      buf GT-FLUSH-U @ + buf GT-TAIL-U @ BYTE-COPY
   then
   GT-TAIL-U @ lenp ! ;

: GT-FLUSH-REMAINDER-FD ( n ptr u8 ptr n -- ) {: fd buf:ptr lenp:ptr :}
   lenp @ GT-TAIL-U !
   GT-TAIL-U @ 0 < if E-STR-BOUNDS throw then
   GT-TAIL-U @ 0= if exit then
   fd buf GT-TAIL-U @ GT-WRITE-FD
   0 lenp ! ;

: GT-CAPTURE-FLUSH-LINES ( -- )
   1 GT-OUT-BUF PROC-OUT-LEN GT-FLUSH-LINES-FD
   2 GT-ERR-BUF PROC-ERR-LEN GT-FLUSH-LINES-FD ;

: GT-CAPTURE-FLUSH-FINAL ( -- )
   1 GT-OUT-BUF PROC-OUT-LEN GT-FLUSH-REMAINDER-FD
   2 GT-ERR-BUF PROC-ERR-LEN GT-FLUSH-REMAINDER-FD ;

: GT-PROGRESS-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   mono-ns GT-PROGRESS-START-NS !
   GT-PROGRESS-START-NS @ GT-PROGRESS-LAST-NS !
   s" RUN: " type label labelu type cr ;

: GT-PROGRESS-ELAPSED-MS ( -- n )
   mono-ns GT-PROGRESS-START-NS @ - PROC-NS-PER-MS / ;

: GT-U-TYPE ( n -- ) {: n :}
   n 0 < if E-TBL-FIELD throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

: GT-PROGRESS-DUE? ( -- bool )
   mono-ns GT-PROGRESS-LAST-NS @ - PROC-NS-PER-MS / GT-HEARTBEAT-MS >= ;

: GT-PROGRESS-WAIT ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-PROGRESS-DUE? if
      mono-ns GT-PROGRESS-LAST-NS !
      s" WAIT: " type label labelu type
      s"  (" type GT-PROGRESS-ELAPSED-MS GT-U-TYPE s" ms)" type cr
   then ;

: GT-PROGRESS-SLICE-MS ( -- ms )
   PROC-REMAINING-MS dup MS>N GT-HEARTBEAT-MS > if drop GT-HEARTBEAT-MS >MS then ;

: GT-PROGRESS-CAPTURE-TIMEOUT? ( ptr u8 n -- bool ) {: label:ptr labelu :}
   PROC-REMAINING-MS MS>N 0 <= if
      PROC-REAP-CAPTURE-TIMEOUT
      0 0=
      exit
   then
   label labelu GT-PROGRESS-WAIT
   0 0= 0= ;

: GT-PROGRESS-CAPTURE-READY ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-CAPTURE-DRAIN
   label labelu GT-PROGRESS-WAIT ;

: GT-PROGRESS-CAPTURE-READY-FLUSH ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-CAPTURE-DRAIN
   GT-CAPTURE-FLUSH-LINES
   label labelu GT-PROGRESS-WAIT ;

: GT-PROGRESS-CAPTURE-STEP? ( ptr u8 n -- bool ) {: label:ptr labelu :}
   GT-PROGRESS-SLICE-MS PROC-POLL-CAPTURE-OUTCOME dup COUNT>N 0= if
      drop
      label labelu GT-PROGRESS-CAPTURE-TIMEOUT?
      exit
   then
   drop
   label labelu GT-PROGRESS-CAPTURE-READY
   0 0= 0= ;

: GT-PROGRESS-CAPTURE-STEP-FLUSH? ( ptr u8 n -- bool ) {: label:ptr labelu :}
   GT-PROGRESS-SLICE-MS PROC-POLL-CAPTURE-OUTCOME dup COUNT>N 0= if
      drop
      label labelu GT-PROGRESS-CAPTURE-TIMEOUT? dup if
         GT-CAPTURE-FLUSH-FINAL
      then
      exit
   then
   drop
   label labelu GT-PROGRESS-CAPTURE-READY-FLUSH
   0 0= 0= ;

: GT-PROGRESS-STDIN-READY ( ptr u8 len ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   in inu PROC-DRIVE-STDIN
   GT-CAPTURE-DRAIN
   label labelu GT-PROGRESS-WAIT ;

: GT-PROGRESS-STDIN-STEP? ( ptr u8 len ptr u8 n -- bool )
   {: in:ptr inu label:ptr labelu :}
   GT-PROGRESS-SLICE-MS PROC-POLL-IO-OUTCOME dup COUNT>N 0= if
      drop
      label labelu GT-PROGRESS-CAPTURE-TIMEOUT?
      exit
   then
   drop
   in inu label labelu GT-PROGRESS-STDIN-READY
   0 0= 0= ;

: GT-PROGRESS-CAPTURE ( ptr u8 n -- ) {: label:ptr labelu :}
   begin PROC-CAPTURE-DONE? 0= while
      label labelu GT-PROGRESS-CAPTURE-STEP? if GT-CAPTURE-STORE exit then
   repeat
   PROC-REAP-CAPTURE
   GT-CAPTURE-STORE ;

: GT-PROGRESS-CAPTURE-FLUSH ( ptr u8 n -- ) {: label:ptr labelu :}
   begin PROC-CAPTURE-DONE? 0= while
      label labelu GT-PROGRESS-CAPTURE-STEP-FLUSH? if GT-CAPTURE-STORE exit then
   repeat
   PROC-REAP-CAPTURE
   GT-CAPTURE-FLUSH-FINAL
   GT-CAPTURE-STORE ;

: GT-PROGRESS-STDIN-CAPTURE ( ptr u8 len ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   inu LEN>N 0 <= if PROC-IN-W PROC-CLOSE-CELL then
   begin PROC-STDIN-CAPTURE-DONE? 0= while
      in inu label labelu GT-PROGRESS-STDIN-STEP? if GT-CAPTURE-STORE exit then
   repeat
   PROC-REAP-CAPTURE
   GT-CAPTURE-STORE ;

: GT-PROGRESS-PASS ( ptr u8 n -- ) {: label:ptr labelu :}
   s" PASS: " type label labelu type
   s"  (" type
   GT-PROGRESS-ELAPSED-MS GT-U-TYPE
   s" ms)" type cr ;

: GT-RC@ ( -- n )
   GT-OUTCOME@ PROC-OUTCOME>RC RC>N ;

: GT-RC= ( n ptr u8 n -- ) {: want name:ptr nameu :}
   GT-RC@ want name nameu GT-CHECK-N ;

: GT-RC-NONZERO ( ptr u8 n -- ) {: name:ptr nameu :}
   GT-RC@ 0 <> name nameu GT-CHECK ;

: GT-TIMEOUT ( ptr u8 n -- ) {: name:ptr nameu :}
   GT-TIMED-OUT @ name nameu GT-CHECK ;

: GT-STDOUT= ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu name:ptr nameu :}
   GT-OUT$ want wantu name nameu GT-CHECK$ ;

: GT-STDERR= ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu name:ptr nameu :}
   GT-ERR$ want wantu name nameu GT-CHECK$ ;

: GT-STDOUT-HAS ( ptr u8 n ptr u8 n -- ) {: needle:ptr needleu name:ptr nameu :}
   GT-OUT$ needle needleu CONTAINS? name nameu GT-CHECK ;

: GT-STDERR-HAS ( ptr u8 n ptr u8 n -- ) {: needle:ptr needleu name:ptr nameu :}
   GT-ERR$ needle needleu CONTAINS? name nameu GT-CHECK ;

: GT-REPORT-OVERFLOW ( -- )
   GT-FAIL# @ GT-FAIL-MAX > if
      s" FAIL: " type
      GT-FAIL# @ GT-FAIL-MAX - GT-U-TYPE
      s"  more failure(s) beyond the " type
      GT-FAIL-MAX GT-U-TYPE
      s" -name cap; see TFAIL records" type cr
   then ;

: GT-REPORT-FAILS ( -- )
   0 begin dup GT-FAIL-STORED < while
      s" FAIL: " type
      dup GT-FAIL-NAME$ type cr
      1+
   repeat drop
   GT-REPORT-OVERFLOW ;

: GT-REPORT ( -- )
   GT-FAIL# @ 0= if
      s" test-runner: ok" type cr
      exit
   then
   s" test-runner: " type GT-FAIL# @ FMT:.INT s"  failure(s)" type cr
   GT-REPORT-FAILS
   s" test-runner: failures" GT-EX-FAIL die ;

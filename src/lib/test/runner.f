\ runner.f - checked process-fixture helpers for native test scripts: GT-START
\ makes a cleanup-tracked root under TMPDIR, GT-RUN captures a child into
\ bounded buffers (output past them throws E-PROC-TRUNCATED), GT-FAIL+ records
\ a named failure and GT-REPORT exits GT-EX-FAIL. STORAGE CLASS. PROCESS-WIDE:
\ the root, capture buffers, failure table and capture frames are static; the
\ standard streams are the process's, so two tasks never capture at once.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/fd.f
require src/lib/test/record.f
require src/lib/fmt.f

2 constant GT-EX-FAIL
256 constant GT-FAIL-MAX
128 constant GT-FAIL-NAME-CAP
32768 constant GT-OUT-CAP
32768 constant GT-ERR-CAP
\ The longest name GT-PATH joins. GT-START keeps that much room below the root
\ it makes, so whether a row's name joins never depends on how long TMPDIR is.
64 constant GT-NAME-MAX

create GT-ROOT-BUF PATH-CAP allot
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

: GT-FAIL-SLOT ( n -- ^u8 ) {: idx :}
   idx 0 < if E-TBL-BOUNDS throw then
   idx GT-FAIL-MAX >= if E-TBL-BOUNDS throw then
   idx GT-FAIL-NAME-CAP * GT-FAIL-NAMES + ;

: GT-FAIL-U-PTR ( n -- ^n ) {: idx :}
   idx 0 < if E-TBL-BOUNDS throw then
   idx GT-FAIL-MAX >= if E-TBL-BOUNDS throw then
   idx cells GT-FAIL-US + ;

: GT-FAIL-NAME$ ( n -- ^u8 n ) {: idx :}
   idx GT-FAIL-SLOT
   idx GT-FAIL-U-PTR @ ;

: GT-RESET ( -- )
   0 GT-OUT-U !
   0 GT-ERR-U !
   0 0= GT-EXITED !
   0 0= 0= GT-TIMED-OUT !
   0 GT-CODE !
   0 GT-FAIL# ! ;

: GT-ROOT ( -- ^u8 n )
   GT-ROOT-BUF GT-ROOT-U @ ;

: GT-OUT$ ( -- ^u8 n )
   GT-OUT-BUF GT-OUT-U @ ;

: GT-ERR$ ( -- ^u8 n )
   GT-ERR-BUF GT-ERR-U @ ;

: GT-FAILURES ( -- n )
   GT-FAIL# @ ;

: GT-EXPECT-ROOT ( -- )
   GT-ROOT-U @ 0 <= if E-FS-PATH throw then ;

: GT-COPY-ROOT! ( ^u8 n -- ) {: a:^ u :}
   u 0 < if E-FS-PATH throw then
   u PATH-CAP > if E-FS-PATH throw then
   a GT-ROOT-BUF u BYTE-COPY
   u GT-ROOT-U ! ;

: GT-CLEANUP ( -- )
   FS:CLEANUP-RUN ;

: GT-PATH ( ^u8 n ^u8 -- n ) {: name:^ nameu dst:^ :}
   GT-EXPECT-ROOT
   nameu GT-NAME-MAX > if E-FS-CAPACITY throw then
   GT-ROOT name nameu dst FS:JOIN-PATH ;

: GT-FAIL-STORED ( -- n )
   GT-FAIL# @ GT-FAIL-MAX > if GT-FAIL-MAX exit then
   GT-FAIL# @ ;

: GT-FAIL-NAME! ( ^u8 n -- ) {: name:^ nameu:n :}
   GT-FAIL# @ GT-FAIL-MAX >= if exit then
   name GT-FAIL# @ GT-FAIL-SLOT nameu BYTE-COPY
   nameu GT-FAIL# @ GT-FAIL-U-PTR ! ;

: GT-FAIL+ ( ^u8 n -- ) {: name:^ nameu:n :}
   nameu 0 < if E-TBL-FIELD throw then
   nameu GT-FAIL-NAME-CAP > if E-TBL-FIELD throw then
   name nameu GT-FAIL-NAME!
   GT-FAIL# @ 1+ GT-FAIL# !
   s" runner" GT-FAIL# @ name nameu TREC-FAIL ;

: GT-CHECK ( bool ^u8 n -- ) {: ok name:^ nameu :}
   ok 0= if name nameu GT-FAIL+ then ;

\ A failure prints both sides, the evidence a bool would lose, on one line
\ after the TFAIL record: FMT:.INT, since `.` ends its text with a newline.
: GT-CHECK-N ( n n ^u8 n -- ) {: got:n want:n name:^ nameu:n :}
   got want <> if
      name nameu GT-FAIL+
      s" runner: expected " type want FMT:.INT
      s"  got " type got FMT:.INT
      cr
   then ;

\ Decompose an ending into the runner's exited/timed-out/code cells (all one
\ cell, lossless: exit codes >= 128 stay distinct from signal deaths).
: GT-OUTCOME! ( PROCESS:ending -- )
   MATCH PROCESS:ending
     exited OF GT-CODE ! 0 0= GT-EXITED ! 0 0= 0= GT-TIMED-OUT ! ENDOF
     signaled OF GT-CODE ! 0 0= 0= GT-EXITED ! 0 0= 0= GT-TIMED-OUT ! ENDOF
     timeout OF 0 GT-CODE ! 0 0= 0= GT-EXITED ! 0 0= GT-TIMED-OUT ! ENDOF
   ;MATCH ;

: GT-OUTCOME@ ( -- PROCESS:ending )
   GT-TIMED-OUT @ if PROCESS-ENDING:TIMEOUT exit then
   GT-EXITED @ if GT-CODE @ PROCESS-ENDING:EXITED exit then
   GT-CODE @ PROCESS-ENDING:SIGNALED ;

: GT-STORE-RUN ( len len PROCESS:ending -- )
   GT-OUTCOME!
   LEN>N GT-ERR-U !
   LEN>N GT-OUT-U ! ;

\ A test's child: path with the argv the caller staged. The child inherits this
\ process's environment, so the scratch it makes lies under the TMPDIR this run
\ was given.
: GT-RUN ( ^u8 n n -- ) {: path:^ pathu timeout :}
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   path pathu >LEN GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN timeout >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   GT-STORE-RUN ;

\ GT-CAPTURE-ACTION points an action's stdout and stderr at two files under the
\ caller's directory and reads them back into the caller's spans; code is the
\ action's throw code, 0 when it returned. Files, not pipes: nothing drains a pipe
\ while the action runs, and the action may run captures of its own through the
\ task's one-capture row (src/lib/process.f). Each file is unlinked once it is open
\ twice (the appending stream, a reader from its start), so no exit path leaves it
\ and a nested capture reuses the name. A nested capture takes the next frame, so
\ each reads its own bytes and the outer one gets its streams back. Held
\ descriptors sit at GT-FD-HOLD-MIN or above and close on exec. Output past a span
\ throws E-PROC-TRUNCATED, even when a child the action left running wrote it
\ after the action returned. Both streams come back on every exit path.
10 constant GT-FD-HOLD-MIN               \ a held descriptor sits above the low ones
2 constant GT-CAP-ROWS                   \ a frame's rows: stdout, then stderr
\ Captures open at once: tests nest two, and one past the bound is refused
\ with E-TBL-BOUNDS before it takes a frame.
8 constant GT-CAP-NEST-MAX

\ Per frame and row, the stream the row saved and the row's file reader, -1
\ until the row holds one.
GT-CAP-NEST-MAX GT-CAP-ROWS * TYPED-BUFFER GT-CAP-SAVES n
GT-CAP-NEST-MAX GT-CAP-ROWS * TYPED-BUFFER GT-CAP-READERS n
variable GT-CAP-DEPTH                    \ frames in use
create GT-CAP-PATH PATH-CAP allot     \ a file's path while GT-CAP-FILE opens it
1 BUFFER: GT-CAP-PROBE                   \ the byte a reader past a full span looks for

\ fd's row in the innermost frame.
: GT-CAP-ROW ( n -- n ) {: fd:n :}
   GT-CAP-DEPTH @ 1- GT-CAP-ROWS * fd FD:STDOUT FD>N - + ;

: GT-CAP-SAVE ( n -- ^n )
   GT-CAP-ROW GT-CAP-SAVES ;

: GT-CAP-READER ( n -- ^n )
   GT-CAP-ROW GT-CAP-READERS ;

: GT-CAP-ENTER ( -- )
   GT-CAP-DEPTH @ GT-CAP-NEST-MAX >= if E-TBL-BOUNDS throw then
   GT-CAP-DEPTH @ 1+ GT-CAP-DEPTH !
   -1 FD:STDOUT FD>N GT-CAP-SAVE !
   -1 FD:STDERR FD>N GT-CAP-SAVE !
   -1 FD:STDOUT FD>N GT-CAP-READER !
   -1 FD:STDERR FD>N GT-CAP-READER ! ;

\ A copy of fd at GT-FD-HOLD-MIN or above, close-on-exec from its first
\ instant; -errno when the host refuses.
: GT-CAP-DUP ( n -- n )
   FD:F-DUPFD-CLOEXEC GT-FD-HOLD-MIN fcntl ;

\ A new empty file named name under dir, open for appending (the writer) and,
\ held, for reading from its start (the reader). It is unlinked before either
\ comes back; on a throw nothing is held.
: GT-CAP-FILE ( ^u8 n ^u8 n -- n n )
   {: dir:^ diru:n name:^ nameu:n :}
   dir diru name nameu GT-CAP-PATH FS:JOIN-PATH {: pathu:n :}
   GT-CAP-PATH pathu FS:>PATHZ
   FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:O-APPEND or FS:MODE-0644 open {: wr:n :}
   wr 0 < if E-FS-OPEN throw then
   GT-CAP-PATH pathu FS:>PATHZ open-rd {: opened:n :}
   GT-CAP-PATH pathu FS:>PATHZ unlink {: gone:n :}
   opened 0 < if wr close E-FS-OPEN throw then
   opened GT-CAP-DUP {: rd:n :}
   opened close
   rd 0 < if wr close E-PROC-OUTPUT throw then
   gone 0 < if wr close rd close E-FS-IO throw then
   wr rd ;

\ fd writes to a new file named name under dir; fd's row keeps the stream fd
\ held before and the file's reader.
: GT-CAP-REDIRECT ( ^u8 n ^u8 n n -- )
   {: dir:^ diru:n name:^ nameu:n fd:n :}
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
   FD:STDOUT FD>N GT-CAP-RELEASE
   FD:STDERR FD>N GT-CAP-RELEASE GT-CAP-FIRST {: rc:n :}
   GT-CAP-DEPTH @ 1- GT-CAP-DEPTH !
   rc 0<> if rc throw then ;

\ The files are read with the action's streams still on them; GT-CAP-LEAVE
\ gives the streams back after the read, on its path and on every throw.
: GT-CAP-RUN ( [ -- ] ^u8 n SPAN:span<u8> SPAN:span<u8> -- len len n )
   {: q dir:^ diru:n out err :}
   dir diru s" capture-out.txt" FD:STDOUT FD>N GT-CAP-REDIRECT
   dir diru s" capture-err.txt" FD:STDERR FD>N GT-CAP-REDIRECT
   q catch {: code:n :}
   FD:STDOUT FD>N GT-CAP-READER @ out GT-CAP-READ
   FD:STDERR FD>N GT-CAP-READER @ err GT-CAP-READ
   code ;

\ Runs the quotation in this process, its stdout and stderr captured into the
\ two spans; a child's capture is GT-RUN.
: GT-CAPTURE-ACTION ( [ -- ] ^u8 n SPAN:span<u8> SPAN:span<u8> -- len len n )
   {: q dir:^ diru:n out err :}
   diru 0 <= if E-FS-PATH throw then
   GT-CAP-ENTER
   q dir diru out err [: GT-CAP-RUN ;] [: GT-CAP-LEAVE ;] finally ;

\ Call before a long subcheck and GT-PROGRESS-PASS after it.
: GT-PROGRESS-RUN ( ^u8 n -- ) {: label:^ labelu :}
   mono-ns GT-PROGRESS-START-NS !
   s" RUN: " type label labelu type cr ;

: GT-PROGRESS-ELAPSED-MS ( -- n )
   mono-ns GT-PROGRESS-START-NS @ - PROCESS:NS-PER-MS / ;

: GT-U-TYPE ( n -- ) {: n :}
   n 0 < if E-TBL-FIELD throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

: GT-PROGRESS-PASS ( ^u8 n -- ) {: label:^ labelu :}
   s" PASS: " type label labelu type
   s"  (" type
   GT-PROGRESS-ELAPSED-MS GT-U-TYPE
   s" ms)" type cr ;

: GT-RC@ ( -- n )
   GT-OUTCOME@ PROCESS:ENDING>RC RC>N ;

: GT-RC= ( n ^u8 n -- ) {: want name:^ nameu :}
   GT-RC@ want name nameu GT-CHECK-N ;

: GT-STDOUT-HAS ( ^u8 n ^u8 n -- ) {: needle:^ needleu name:^ nameu :}
   GT-OUT$ needle needleu CONTAINS? name nameu GT-CHECK ;

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

\ The longest TMPDIR GT-START takes for a root prefix of n bytes: the root
\ FS:MKTEMP-DIR makes under it, a slash and a GT-NAME-MAX name fit PATH-CAP.
: GT-TMPDIR-MAX ( n -- n )
   FS:TEMP-DIR-BASE-MAX 1- GT-NAME-MAX - ;

\ A TMPDIR longer than that is refused once, here, before anything is made: a
\ named failure with its length and the limit, then the report's exit. A row's
\ own deeper paths are not checked here: the deepest, streaming-sha256's deep
\ file (tools/sha256-file-test.f), must still fit PATH-CAP (src/lib/fs.f
\ PATHZ-INTO), so a gate TMPDIR must be short enough to leave it room. An
\ empty TMPDIR counts as the /tmp FS:MKTEMP-DIR then uses.
: GT-ROOM-CHECK ( n -- ) {: prefixu:n :}
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then nip {: tmpu:n :}
   prefixu GT-TMPDIR-MAX {: max:n :}
   tmpu max <= if exit then
   s" TMPDIR leaves the test root no room" GT-FAIL+
   s" runner: TMPDIR is " type tmpu FMT:.INT
   s"  bytes; at most " type max FMT:.INT
   s"  leave the root and a " type GT-NAME-MAX FMT:.INT
   s" -byte name within PATH-CAP " type PATH-CAP FMT:.INT
   cr
   GT-REPORT ;

: GT-START ( ^u8 n -- ) {: prefix:^ prefixu :}
   GT-RESET
   FS:CLEANUP-RESET
   0 GT-ROOT-U !
   prefixu GT-ROOM-CHECK
   prefix prefixu FS:MKTEMP-DIR GT-COPY-ROOT!
   GT-ROOT FS:CLEANUP-TREE+ ;

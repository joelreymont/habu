\ runner.f - checked native test runner foundation.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, and lib/process-argv.f.

require lib/test/record.f

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
   prefix prefixu TMPDIR-MKDIR GT-COPY-ROOT!
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

: GT-FLUSH-LINES-FD ( n ptr u8 ptr a -- ) {: fd buf:ptr lenp:ptr :}
   lenp @ 0 < if E-STR-BOUNDS throw then
   buf lenp @ GT-LINE-FLUSH-U GT-FLUSH-U !
   GT-FLUSH-U @ 0 <= if exit then
   fd buf GT-FLUSH-U @ GT-WRITE-FD
   lenp @ GT-FLUSH-U @ - GT-TAIL-U !
   GT-TAIL-U @ 0 > if
      buf GT-FLUSH-U @ + buf GT-TAIL-U @ BYTE-COPY
   then
   GT-TAIL-U @ lenp ! ;

: GT-FLUSH-REMAINDER-FD ( n ptr u8 ptr a -- ) {: fd buf:ptr lenp:ptr :}
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

: GT-PROGRESS-STDIN-TIMEOUT? ( ptr u8 n -- bool ) {: label:ptr labelu :}
   PROC-REMAINING-MS MS>N 0 <= if
      PROC-CLOSE-STDIN-FDS
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
      label labelu GT-PROGRESS-STDIN-TIMEOUT?
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
   GT-RC@ want = name nameu GT-CHECK ;

: GT-RC-NONZERO ( ptr u8 n -- ) {: name:ptr nameu :}
   GT-RC@ 0 <> name nameu GT-CHECK ;

: GT-TIMEOUT ( ptr u8 n -- ) {: name:ptr nameu :}
   GT-TIMED-OUT @ name nameu GT-CHECK ;

: GT-STDOUT= ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu name:ptr nameu :}
   GT-OUT$ want wantu STR= name nameu GT-CHECK ;

: GT-STDERR= ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu name:ptr nameu :}
   GT-ERR$ want wantu STR= name nameu GT-CHECK ;

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
   s" test-runner: " type GT-FAIL# @ . s" failure(s)" type cr
   GT-REPORT-FAILS
   s" test-runner: failures" GT-EX-FAIL die ;

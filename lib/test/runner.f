\ runner.f - checked native test runner foundation.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, and lib/process-argv.f.

require lib/test/record.f

package GATE

public

2 constant GT-EX-FAIL
256 constant GT-FAIL-MAX
128 constant GT-FAIL-NAME-CAP
32768 constant GT-OUT-CAP
32768 constant GT-ERR-CAP
10000 constant GT-DEFAULT-TIMEOUT-MS
5000 constant GT-HEARTBEAT-MS

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

\ Has a temp root been made yet?  Every caller that used to read GT-ROOT-U for
\ this asks the question instead of the cell.
: GT-ROOT? ( -- bool )
   GT-ROOT-U @ 0 > ;

: GT-OUT$ ( -- ptr u8 n )
   GT-OUT-BUF GT-OUT-U @ ;

: GT-ERR$ ( -- ptr u8 n )
   GT-ERR-BUF GT-ERR-U @ ;

\ The stdout and stderr sinks: the buffer a producer may fill, paired with the
\ capacity that belongs to it.  Lend these instead of hand-pairing a buffer with
\ a cap - one word cannot transpose the pair, and the storage stays the
\ runner's.  GT-OUT$/GT-ERR$ read back what a producer wrote here.
: GT-OUT-SINK ( -- ptr u8 len )
   GT-OUT-BUF GT-OUT-CAP >LEN ;

: GT-ERR-SINK ( -- ptr u8 len )
   GT-ERR-BUF GT-ERR-CAP >LEN ;

: GT-FAILURES ( -- n )
   GT-FAIL# @ ;

: GT-EXPECT-ROOT ( -- )
   GT-ROOT? 0= if E-FS-PATH throw then ;

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

\ Did the last run hit the capture deadline?  Asked of the outcome, which is the
\ one tag authority; GT-RC@ cannot answer it, because a timeout and a SIGKILL
\ death both flatten to rc 137.
: GT-TIMED-OUT? ( -- bool )
   GT-OUTCOME@ MATCH outcome
     exited OF drop 0 0= 0= ENDOF
     signaled OF drop 0 0= 0= ENDOF
     timeout OF 0 0= ENDOF
   ;MATCH ;

: GT-STORE-RUN ( len len outcome -- )
   GT-OUTCOME!
   LEN>N GT-ERR-U !
   LEN>N GT-OUT-U ! ;

: GT-CAPTURE-STORE ( -- )
   PROC-CAPTURE-OUTCOME@ GT-STORE-RUN ;

: GT-RUN ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   path pathu >LEN GT-OUT-SINK GT-ERR-SINK timeout >MS
   RUN-ARGV-CAPTURE-OUTCOME
   GT-STORE-RUN ;

: GT-CAPTURE-DRAIN ( -- )
   GT-OUT-SINK GT-ERR-SINK PROC-DRAIN-READY ;

: GT-RUN-DEFAULT ( ptr u8 n -- )
   GT-DEFAULT-TIMEOUT-MS GT-RUN ;

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

: GT-PROGRESS-CAPTURE-STEP? ( ptr u8 n -- bool ) {: label:ptr labelu :}
   GT-PROGRESS-SLICE-MS PROC-POLL-CAPTURE-OUTCOME dup COUNT>N 0= if
      drop
      label labelu GT-PROGRESS-CAPTURE-TIMEOUT?
      exit
   then
   drop
   label labelu GT-PROGRESS-CAPTURE-READY
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
   GT-TIMED-OUT? name nameu GT-CHECK ;

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

;package

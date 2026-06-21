\ test-runner.f - checked native gate runner foundation.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, and lib/process-argv.f.

2 constant GT-EX-FAIL
64 constant GT-FAIL-MAX
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
variable GT-OUTCOME-KIND
variable GT-OUTCOME-CODE
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
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   0 GT-OUTCOME-CODE !
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

: GT-FAIL+ ( ptr u8 n -- ) {: name:ptr nameu :}
   GT-FAIL# @ GT-FAIL-MAX >= if E-TBL-BOUNDS throw then
   nameu 0 < if E-TBL-FIELD throw then
   nameu GT-FAIL-NAME-CAP > if E-TBL-FIELD throw then
   name GT-FAIL# @ GT-FAIL-SLOT nameu BYTE-COPY
   nameu GT-FAIL# @ GT-FAIL-U-PTR !
   GT-FAIL# @ 1+ GT-FAIL# ! ;

: GT-CHECK ( bool ptr u8 n -- ) {: ok name:ptr nameu :}
   ok 0= if name nameu GT-FAIL+ then ;

: GT-RUN ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   path pathu GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP timeout
   RUN-ARGV-CAPTURE-OUTCOME
   GT-OUTCOME-CODE !
   GT-OUTCOME-KIND !
   GT-ERR-U !
   GT-OUT-U ! ;

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

: GT-PROGRESS-SLICE-MS ( -- n )
   PROC-REMAINING-MS dup GT-HEARTBEAT-MS > if drop GT-HEARTBEAT-MS then ;

: GT-PROGRESS-PASS ( ptr u8 n -- ) {: label:ptr labelu :}
   s" PASS: " type label labelu type
   s"  (" type
   GT-PROGRESS-ELAPSED-MS GT-U-TYPE
   s" ms)" type cr ;

: GT-RC@ ( -- n )
   GT-OUTCOME-KIND @ PROC-OUTCOME-EXIT = if
      GT-OUTCOME-CODE @ exit
   then
   128 GT-OUTCOME-CODE @ + ;

: GT-RC= ( n ptr u8 n -- ) {: want name:ptr nameu :}
   GT-RC@ want = name nameu GT-CHECK ;

: GT-RC-NONZERO ( ptr u8 n -- ) {: name:ptr nameu :}
   GT-RC@ 0 <> name nameu GT-CHECK ;

: GT-TIMEOUT ( ptr u8 n -- ) {: name:ptr nameu :}
   GT-OUTCOME-KIND @ PROC-OUTCOME-TIMEOUT = name nameu GT-CHECK ;

: GT-STDOUT= ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu name:ptr nameu :}
   GT-OUT$ want wantu STR= name nameu GT-CHECK ;

: GT-STDERR= ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu name:ptr nameu :}
   GT-ERR$ want wantu STR= name nameu GT-CHECK ;

: GT-STDOUT-HAS ( ptr u8 n ptr u8 n -- ) {: needle:ptr needleu name:ptr nameu :}
   GT-OUT$ needle needleu CONTAINS? name nameu GT-CHECK ;

: GT-STDERR-HAS ( ptr u8 n ptr u8 n -- ) {: needle:ptr needleu name:ptr nameu :}
   GT-ERR$ needle needleu CONTAINS? name nameu GT-CHECK ;

: GT-REPORT-FAILS ( -- )
   0 begin dup GT-FAIL# @ < while
      s" FAIL: " type
      dup GT-FAIL-NAME$ type cr
      1+
   repeat drop ;

: GT-REPORT ( -- )
   GT-FAIL# @ 0= if
      s" test-runner: ok" type cr
      exit
   then
   s" test-runner: " type GT-FAIL# @ . s" failure(s)" type cr
   GT-REPORT-FAILS
   s" test-runner: failures" GT-EX-FAIL die ;

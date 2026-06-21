\ run-attempts-lib.f - checked candidate enumeration for attempt runners.
\
\ Load after lib/errors.f, lib/string.f, lib/json-write.f, tools/json.f,
\ lib/fs.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ bench/llm/manifest.f, and bench/llm/diagnostic-stats.f.

64 constant RA-ROUND-MAX
20 constant RA-NUM-CAP
$4000 constant RA-CAPTURE-CAP
$8000 constant RA-DIAG-CAP
$8000 constant RA-EVENT-CAP
$20000 constant RA-BUNDLE-CAP
$10000 constant RA-SRC-CAP
10000 constant RA-DEFAULT-TIMEOUT-MS
5 constant RA-TRUST-LEN
46 constant RA-DOT
102 constant RA-F
40 constant RA-LPAREN
41 constant RA-RPAREN
45 constant RA-HYPHEN
58 constant RA-COLON
95 constant RA-UNDERSCORE

-3240 constant E-RA-CAPACITY
-3241 constant E-RA-MISSING

create RA-ROUND-PATHS RA-ROUND-MAX FS-PATH-CAP * allot
create RA-ROUND-US RA-ROUND-MAX cells allot
create RA-TMP-PATH FS-PATH-CAP allot
create RA-REF-PATH FS-PATH-CAP allot
create RA-NAME-BUF FS-PATH-CAP allot
create RA-NUM-BUF RA-NUM-CAP allot
create RA-BUNDLE-BUF RA-BUNDLE-CAP allot
create RA-SRC-BUF RA-SRC-CAP allot
create RA-OUT-BUF RA-CAPTURE-CAP allot
create RA-ERR-BUF RA-CAPTURE-CAP allot
create RA-DIAG-BUF RA-DIAG-CAP allot
create RA-EVENT-BUF RA-EVENT-CAP allot
create RA-FINAL-PATH FS-PATH-CAP allot
create RA-FIRST-BAD-PATH FS-PATH-CAP allot

variable RA-ROUND#
variable RA-TMP-U
variable RA-REF-U
variable RA-NAME-U
variable RA-NUM-I
variable RA-I
variable RA-NEXT
variable RA-BUNDLE-U
variable RA-TARGET-SEEN
variable RA-OUT-U
variable RA-ERR-U
variable RA-RC
variable RA-TIMEOUT-MS
variable RA-DIAG-U
variable RA-EVENT-U
variable RA-FINAL-U
variable RA-FIRST-BAD-U
variable RA-CHECKER-N
variable RA-FIRST-CHECKER-OK
variable RA-FIRST-TESTS-OK
variable RA-TESTS-OK
variable RA-FINAL-CHARS-N
variable RA-TRUST-USES-N
variable RA-SIGNATURE-WEAK
variable RA-ALL-STABLE
variable RA-SIG-OPEN
variable RA-SIG-CLOSE

: RA-DEFAULT-TIMEOUT! ( -- )
   RA-DEFAULT-TIMEOUT-MS RA-TIMEOUT-MS ! ;

RA-DEFAULT-TIMEOUT!

: RA-TRUE ( -- bool )
   0 0= ;

: RA-FALSE ( -- bool )
   RA-TRUE 0= ;

: RA-CHECK-ROUND ( n -- ) {: idx :}
   idx 0 < if E-RA-CAPACITY throw then
   idx RA-ROUND-MAX >= if E-RA-CAPACITY throw then ;

: RA-ROUND-SLOT ( n -- ptr u8 ) {: idx :}
   idx RA-CHECK-ROUND
   RA-ROUND-PATHS idx FS-PATH-CAP * + ;

: RA-ROUND-U-PTR ( n -- ptr n ) {: idx :}
   idx RA-CHECK-ROUND
   RA-ROUND-US idx cells + ;

: RA-ROUND$ ( n -- ptr u8 n ) {: idx :}
   idx RA-ROUND-SLOT idx RA-ROUND-U-PTR @ ;

: RA-CHECK-PATH-U ( n -- ) {: u :}
   u 0 < if E-RA-CAPACITY throw then
   u FS-PATH-CAP > if E-RA-CAPACITY throw then ;

: RA-RESET ( -- )
   0 RA-ROUND# !
   0 RA-TMP-U !
   0 RA-REF-U !
   0 RA-NAME-U ! ;

: RA-BUNDLE-RESET ( -- )
   0 RA-BUNDLE-U !
   0 RA-TARGET-SEEN ! ;

: RA-NAME-ROOM ( n -- ) {: add :}
   add 0 < if E-RA-CAPACITY throw then
   add FS-PATH-CAP RA-NAME-U @ - > if E-RA-CAPACITY throw then ;

: RA-NAME+ ( ptr u8 n -- ) {: a:ptr u :}
   u RA-NAME-ROOM
   a RA-NAME-BUF RA-NAME-U @ + u BYTE-COPY
   RA-NAME-U @ u + RA-NAME-U ! ;

: RA-NAME-C ( n -- ) {: c :}
   1 RA-NAME-ROOM
   c RA-NAME-BUF RA-NAME-U @ + c!
   RA-NAME-U @ 1+ RA-NAME-U ! ;

: RA-NAME-U+ ( n -- ) {: u :}
   u 0 < if E-RA-CAPACITY throw then
   RA-NUM-CAP RA-NUM-I !
   u 0= if s" 0" RA-NAME+ exit then
   u begin dup 0 > while
      dup 10 mod 48 +
      RA-NUM-I @ 1- RA-NUM-I !
      RA-NUM-BUF RA-NUM-I @ + c!
      10 /
   repeat drop
   RA-NUM-BUF RA-NUM-I @ + RA-NUM-CAP RA-NUM-I @ - RA-NAME+ ;

: RA-NAME$ ( -- ptr u8 n )
   RA-NAME-BUF RA-NAME-U @ ;

: RA-ROUND-NAME! ( n -- ) {: round :}
   round 0 <= if E-RA-CAPACITY throw then
   0 RA-NAME-U !
   round RA-NAME-U+
   RA-DOT RA-NAME-C
   RA-F RA-NAME-C ;

: RA-SUFFIX-F-PATH ( ptr u8 n ptr u8 -- n ) {: a:ptr u dst:ptr :}
   u 2 + RA-CHECK-PATH-U
   a dst u BYTE-COPY
   RA-DOT dst u + c!
   RA-F dst u 1+ + c!
   u 2 + ;

: RA-BUNDLE-ROOM ( n -- ) {: add :}
   add 0 < if E-RA-CAPACITY throw then
   add RA-BUNDLE-CAP RA-BUNDLE-U @ - > if E-RA-CAPACITY throw then ;

: RA-BUNDLE+ ( ptr u8 n -- ) {: a:ptr u :}
   u RA-BUNDLE-ROOM
   a RA-BUNDLE-BUF RA-BUNDLE-U @ + u BYTE-COPY
   RA-BUNDLE-U @ u + RA-BUNDLE-U ! ;

: RA-BUNDLE-C ( n -- ) {: c :}
   1 RA-BUNDLE-ROOM
   c RA-BUNDLE-BUF RA-BUNDLE-U @ + c!
   RA-BUNDLE-U @ 1+ RA-BUNDLE-U ! ;

: RA-BUNDLE$ ( -- ptr u8 n )
   RA-BUNDLE-BUF RA-BUNDLE-U @ ;

: RA-DIAGS$ ( -- ptr u8 n )
   RA-DIAG-BUF RA-DIAG-U @ ;

: RA-EVENTS$ ( -- ptr u8 n )
   RA-EVENT-BUF RA-EVENT-U @ ;

: RA-FINAL$ ( -- ptr u8 n )
   RA-FINAL-PATH RA-FINAL-U @ ;

: RA-FIRST-BAD$ ( -- ptr u8 n )
   RA-FIRST-BAD-PATH RA-FIRST-BAD-U @ ;

: RA-OUT$ ( -- ptr u8 n )
   RA-OUT-BUF RA-OUT-U @ ;

: RA-ERR$ ( -- ptr u8 n )
   RA-ERR-BUF RA-ERR-U @ ;

: RA-RC@ ( -- n )
   RA-RC @ ;

: RA-CHECKERS ( -- n )
   RA-CHECKER-N @ ;

: RA-REPAIRS ( -- n )
   RA-CHECKER-N @ 0 > if RA-CHECKER-N @ 1- exit then
   0 ;

: RA-FIRST-TESTS? ( -- bool )
   RA-FIRST-TESTS-OK @ 0 <> ;

: RA-TESTS-PASSED? ( -- bool )
   RA-TESTS-OK @ 0 <> ;

: RA-SIGNATURE-WEAKENED? ( -- bool )
   RA-SIGNATURE-WEAK @ 0 <> ;

: RA-ALL-ERRORS-STABLE? ( -- bool )
   RA-ALL-STABLE @ 0 <> ;

: RA-FINAL-CHARS ( -- n )
   RA-FINAL-CHARS-N @ ;

: RA-TRUST-USES ( -- n )
   RA-TRUST-USES-N @ ;

: RA-FIRST-CHECKER$ ( -- ptr u8 n )
   RA-FIRST-CHECKER-OK @ 0 <> if s" certified" exit then
   s" rejected" ;

: RA-CHECKER++ ( -- )
   RA-CHECKER-N @ 1+ RA-CHECKER-N ! ;

: RA-ROW-RESET ( -- )
   0 RA-DIAG-U !
   0 RA-EVENT-U !
   0 RA-FINAL-U !
   0 RA-FIRST-BAD-U !
   0 RA-CHECKER-N !
   0 RA-FIRST-CHECKER-OK !
   0 RA-FIRST-TESTS-OK !
   0 RA-TESTS-OK !
   0 RA-FINAL-CHARS-N !
   0 RA-TRUST-USES-N !
   0 RA-SIGNATURE-WEAK !
   -1 RA-ALL-STABLE ! ;

: RA-BUF-ROOM ( n n n -- ) {: cap used add :}
   add 0 < if E-RA-CAPACITY throw then
   add cap used - > if E-RA-CAPACITY throw then ;

: RA-DIAG-ROOM ( n -- ) {: add :}
   RA-DIAG-CAP RA-DIAG-U @ add RA-BUF-ROOM ;

: RA-EVENT-ROOM ( n -- ) {: add :}
   RA-EVENT-CAP RA-EVENT-U @ add RA-BUF-ROOM ;

: RA-DIAG+ ( ptr u8 n -- ) {: a:ptr u :}
   u RA-DIAG-ROOM
   a RA-DIAG-BUF RA-DIAG-U @ + u BYTE-COPY
   RA-DIAG-U @ u + RA-DIAG-U ! ;

: RA-EVENT+ ( ptr u8 n -- ) {: a:ptr u :}
   u RA-EVENT-ROOM
   a RA-EVENT-BUF RA-EVENT-U @ + u BYTE-COPY
   RA-EVENT-U @ u + RA-EVENT-U ! ;

: RA-COPY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u RA-CHECK-PATH-U
   a dst u BYTE-COPY
   u lenp ! ;

: RA-SET-FINAL ( ptr u8 n -- )
   RA-FINAL-PATH RA-FINAL-U RA-COPY-PATH! ;

: RA-SET-FIRST-BAD ( ptr u8 n -- )
   RA-FIRST-BAD-PATH RA-FIRST-BAD-U RA-COPY-PATH! ;

: RA-TASK-DIR! ( ptr u8 n ptr u8 n -- ) {: root:ptr rootu id:ptr idu :}
   root rootu id idu RA-TMP-PATH JOIN-PATH RA-TMP-U ! ;

: RA-TASK-DIR$ ( -- ptr u8 n )
   RA-TMP-PATH RA-TMP-U @ ;

: RA-REF$ ( -- ptr u8 n )
   RA-REF-PATH RA-REF-U @ ;

: RA-DIR-ID-FILE! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: dir:ptr diru id:ptr idu dst:ptr lenp:ptr :}
   dir diru id idu RA-TMP-PATH JOIN-PATH {: baseu :}
   RA-TMP-PATH baseu dst RA-SUFFIX-F-PATH lenp ! ;

: RA-SINGLE-PATH! ( ptr u8 n ptr u8 n n -- ) {: root:ptr rootu id:ptr idu idx :}
   root rootu id idu idx RA-ROUND-SLOT idx RA-ROUND-U-PTR RA-DIR-ID-FILE! ;

: RA-ROUND-PATH! ( ptr u8 n n n -- ) {: dir:ptr diru round idx :}
   round RA-ROUND-NAME!
   dir diru RA-NAME$ idx RA-ROUND-SLOT JOIN-PATH
   idx RA-ROUND-U-PTR ! ;

: RA-MAYBE-ADD-ROUND ( ptr u8 n n -- ) {: dir:ptr diru round :}
   RA-ROUND# @ {: idx :}
   idx RA-CHECK-ROUND
   dir diru round idx RA-ROUND-PATH!
   idx RA-ROUND$ FILE? if
      idx 1+ RA-ROUND# !
   then ;

: RA-REQUIRE-ROUNDS ( -- )
   RA-ROUND# @ 0= if E-RA-MISSING throw then ;

: RA-REQUIRE-SINGLE ( -- )
   0 RA-ROUND$ FILE? 0= if E-RA-MISSING throw then ;

: RA-ENUM-ROUND-DIR ( ptr u8 n -- n ) {: dir:ptr diru :}
   1 RA-I !
   begin RA-I @ RA-ROUND-MAX <= while
      dir diru RA-I @ RA-MAYBE-ADD-ROUND
      RA-I @ 1+ RA-I !
   repeat
   RA-REQUIRE-ROUNDS
   RA-ROUND# @ ;

: RA-CANDIDATES ( ptr u8 n ptr u8 n -- n ) {: root:ptr rootu id:ptr idu :}
   RA-RESET
   root rootu id idu RA-TASK-DIR!
   RA-TASK-DIR$ DIR? if
      RA-TASK-DIR$ RA-ENUM-ROUND-DIR exit
   then
   root rootu id idu 0 RA-SINGLE-PATH!
   RA-REQUIRE-SINGLE
   1 RA-ROUND# !
   1 ;

: RA-READ-SOURCE ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu :}
   path pathu FILE? 0= if E-RA-MISSING throw then
   path pathu RA-SRC-BUF RA-SRC-CAP READ-ALL
   RA-SRC-BUF swap ;

: RA-APPEND-FILE ( ptr u8 n -- )
   RA-READ-SOURCE RA-BUNDLE+ ;

: RA-APPEND-FILE-LN ( ptr u8 n -- )
   RA-APPEND-FILE
   STR-LF RA-BUNDLE-C ;

: RA-REF-PATH! ( ptr u8 n ptr u8 n -- ) {: ref:ptr refu id:ptr idu :}
   ref refu id idu RA-REF-PATH RA-REF-U RA-DIR-ID-FILE! ;

: RA-APPEND-REF-LN ( ptr u8 n ptr u8 n -- ) {: ref:ptr refu id:ptr idu :}
   ref refu id idu RA-REF-PATH!
   RA-REF$ RA-APPEND-FILE-LN ;

: RA-LINE-ID$ ( ptr u8 n -- ptr u8 n )
   BM-T-ID BM-TASK-FIELD$ ;

: RA-APPEND-TASK-SOURCE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: line:ptr lineu ref:ptr refu target:ptr targetu cand:ptr candu :}
   line lineu BM-BLANK-OR-COMMENT? if exit then
   line lineu RA-LINE-ID$ target targetu STR= if
      -1 RA-TARGET-SEEN !
      cand candu RA-APPEND-FILE-LN
   else
      ref refu line lineu RA-LINE-ID$ RA-APPEND-REF-LN
   then ;

: RA-REQUIRE-TARGET ( -- )
   RA-TARGET-SEEN @ 0= if E-RA-MISSING throw then ;

: RA-BUILD-BUNDLE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: tasks:ptr tasksu ref:ptr refu target:ptr targetu cand:ptr candu tests:ptr testsu :}
   RA-BUNDLE-RESET
   0 RA-NEXT !
   begin tasks tasksu RA-NEXT @ BM-LINE-NEXT while
      RA-NEXT !
      ref refu target targetu cand candu RA-APPEND-TASK-SOURCE
   repeat drop 2drop
   RA-REQUIRE-TARGET
   tests testsu RA-APPEND-FILE
   RA-BUNDLE$ ;

: RA-CHECK-ARGV ( ptr u8 n -- ) {: cand:ptr candu :}
   PROC-ARGV-ENV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/source.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/check.f" PROC-ARGV+
   s" --" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   s" --all-errors" PROC-ARGV+
   cand candu PROC-ARGV+ ;

: RA-HB-CAPTURE ( -- )
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" RA-OUT-BUF RA-CAPTURE-CAP RA-ERR-BUF RA-CAPTURE-CAP
   RA-TIMEOUT-MS @ RUN-ARGV-ENV-CAPTURE
   RA-RC !
   RA-ERR-U !
   RA-OUT-U ! ;

: RA-HB-STDIN-CAPTURE ( ptr u8 n -- ) {: in:ptr inu :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" in inu RA-OUT-BUF RA-CAPTURE-CAP RA-ERR-BUF RA-CAPTURE-CAP
   RA-TIMEOUT-MS @ RUN-ARGV-ENV-STDIN-CAPTURE
   RA-RC !
   RA-ERR-U !
   RA-OUT-U ! ;

: RA-CHECK-CANDIDATE ( ptr u8 n -- bool )
   RA-CHECK-ARGV
   RA-HB-CAPTURE
   RA-RC @ 0= ;

: RA-TEST-OUT-OK? ( -- bool )
   RA-RC @ 0 <> if 0 0= 0= exit then
   RA-OUT$ s" ok" STR= ;

: RA-RUN-BUNDLE-TESTS ( ptr u8 n -- bool )
   RA-HB-STDIN-CAPTURE
   RA-TEST-OUT-OK? ;

: RA-RUN-CANDIDATE-TESTS ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   RA-BUILD-BUNDLE
   RA-RUN-BUNDLE-TESTS ;

: RA-RECORD-CERTIFIED ( ptr u8 n -- ) {: cand:ptr candu :}
   RA-CHECKER-N @ 1 = if -1 RA-FIRST-CHECKER-OK ! then
   cand candu RA-SET-FINAL ;

: RA-RECORD-TEST-PASS ( ptr u8 n -- ) {: cand:ptr candu :}
   RA-CHECKER-N @ 1 = if -1 RA-FIRST-TESTS-OK ! then
   -1 RA-TESTS-OK !
   cand candu RA-SET-FINAL ;

: RA-RECORD-TEST-FAIL ( ptr u8 n -- )
   RA-SET-FINAL ;

: RA-RECORD-REJECT ( ptr u8 n n -- ) {: cand:ptr candu round :}
   RA-CHECKER-N @ 1 = if cand candu RA-SET-FIRST-BAD then
   cand candu RA-SET-FINAL
   RA-ERR$ RA-DIAG+
   RA-ERR$ round DGS-EVENTS-FROM-DIAGS$ RA-EVENT+ ;

: RA-SCAN-DIAGS ( -- )
   RA-DIAGS$ DGS-SCAN-DIAGS ;

: RA-DIAGNOSTIC-COUNT ( -- n )
   RA-SCAN-DIAGS
   DGS-DIAGNOSTIC-COUNT ;

: RA-DIAGNOSTIC-TOKEN? ( -- bool )
   RA-SCAN-DIAGS
   DGS-DIAGNOSTIC-TOKEN? ;

: RA-DIAGNOSTIC-SPAN? ( -- bool )
   RA-SCAN-DIAGS
   DGS-DIAGNOSTIC-SPAN? ;

: RA-DIAGNOSTIC-EXPECTED? ( -- bool )
   RA-SCAN-DIAGS
   DGS-DIAGNOSTIC-EXPECTED? ;

: RA-DIAGNOSTIC-ACTUAL? ( -- bool )
   RA-SCAN-DIAGS
   DGS-DIAGNOSTIC-ACTUAL? ;

: RA-DIAGNOSTIC-CODE? ( -- bool )
   RA-SCAN-DIAGS
   DGS-DIAGNOSTIC-CODE? ;

: RA-DIAGNOSTIC-REPAIR-CLASS? ( -- bool )
   RA-SCAN-DIAGS
   DGS-DIAGNOSTIC-REPAIR-CLASS? ;

: RA-REPAIR-STATS$ ( -- ptr u8 n )
   RA-EVENTS$ RA-TESTS-PASSED? 0 DGS-REPAIR-STATS$ ;

: RA-BETWEEN? ( n n n -- bool ) {: c lo hi :}
   c lo >= c hi <= and ;

: RA-ALNUM? ( n -- bool ) {: c :}
   c 65 90 RA-BETWEEN?
   c 97 122 RA-BETWEEN? or
   c 48 57 RA-BETWEEN? or ;

: RA-WORD-CHAR? ( n -- bool ) {: c :}
   c RA-ALNUM?
   c RA-UNDERSCORE = or
   c RA-HYPHEN = or ;

: RA-BEFORE-BOUNDARY? ( ptr u8 n n -- bool ) {: a:ptr u idx :}
   idx 0= if RA-TRUE exit then
   a idx 1- + c@ RA-WORD-CHAR? 0= ;

: RA-AFTER-BOUNDARY? ( ptr u8 n n -- bool ) {: a:ptr u idx :}
   idx RA-TRUST-LEN + u >= if RA-TRUE exit then
   a idx RA-TRUST-LEN + + c@ RA-WORD-CHAR? 0= ;

: RA-TRUST-AT? ( ptr u8 n n -- bool ) {: a:ptr u idx :}
   idx 0 < if RA-FALSE exit then
   idx RA-TRUST-LEN + u > if RA-FALSE exit then
   a idx + RA-TRUST-LEN s" trust" STR=CI 0= if RA-FALSE exit then
   a u idx RA-BEFORE-BOUNDARY? 0= if RA-FALSE exit then
   a u idx RA-AFTER-BOUNDARY? ;

: RA-LINE-HAS-TRUST? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 begin dup u < while
      dup a u rot RA-TRUST-AT? if drop RA-TRUE exit then
      1+
   repeat drop RA-FALSE ;

: RA-TRUST-LINES ( ptr u8 n -- n ) {: a:ptr u :}
   0
   0 RA-NEXT !
   begin a u RA-NEXT @ BM-LINE-NEXT while
      RA-NEXT !
      RA-LINE-HAS-TRUST? if 1+ then
   repeat drop 2drop ;

: RA-FIRST-DEF-LINE? ( ptr u8 n -- bool ) {: line:ptr lineu :}
   lineu 0= if RA-FALSE exit then
   line c@ RA-COLON = ;

: RA-LINE-SIG$ ( ptr u8 n -- ptr u8 n ) {: line:ptr lineu :}
   line lineu RA-LPAREN INDEX-OF RA-SIG-OPEN !
   RA-SIG-OPEN @ 0 < if line 0 exit then
   line RA-SIG-OPEN @ 1+ + lineu RA-SIG-OPEN @ 1+ - RA-RPAREN INDEX-OF RA-SIG-CLOSE !
   RA-SIG-CLOSE @ 0 < if line 0 exit then
   line RA-SIG-OPEN @ 1+ + RA-SIG-CLOSE @ TRIM ;

: RA-FILE-SIG$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 RA-NEXT !
   begin a u RA-NEXT @ BM-LINE-NEXT while
      RA-NEXT !
      2dup RA-FIRST-DEF-LINE? if RA-LINE-SIG$ exit then
      2drop
   repeat drop 2drop
   a 0 ;

: RA-SIGNATURE-WEAK? ( ptr u8 n ptr u8 n -- bool )
   {: src:ptr srcu exp:ptr expu :}
   src srcu RA-FILE-SIG$ exp expu STR= 0= ;

: RA-FINAL-METRICS! ( ptr u8 n -- ) {: exp:ptr expu :}
   RA-FINAL$ RA-READ-SOURCE {: src:ptr srcu :}
   srcu RA-FINAL-CHARS-N !
   src srcu RA-TRUST-LINES RA-TRUST-USES-N !
   src srcu exp expu RA-SIGNATURE-WEAK? if -1 else 0 then RA-SIGNATURE-WEAK ! ;

: RA-ALL-ERRORS-STABLE! ( -- )
   RA-DIAG-U @ 0= if -1 RA-ALL-STABLE ! exit then
   RA-FIRST-BAD-U @ 0= if -1 RA-ALL-STABLE ! exit then
   RA-FIRST-BAD$ RA-CHECK-CANDIDATE if 0 RA-ALL-STABLE ! exit then
   RA-ERR$ RA-DIAGS$ STR= if -1 else 0 then RA-ALL-STABLE ! ;

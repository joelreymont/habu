\ run-result-cache-test.f - focused tests for the gate phase result cache.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\   lib/fs.f lib/fs-mutate.f lib/content-key.f test/run-files.f
\   test/run-result-cache.f test/run-result-cache-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require test/run-files.f
require test/run-result-cache.f

package RCT

$100000 constant FILE-CAP
$8000 constant SET-CAP
64 constant KEY-U
$09 constant TAB-C
$20 constant BL-C
$22 constant DQ-C

create ROOT-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create CACHE-BUF FS-PATH-CAP allot
create KEY1 80 allot
create KEY2 80 allot
create SET-BUF SET-CAP allot

variable ROOT-LEN
variable SRC-LEN
variable CACHE-LEN
variable SET-LEN
variable SET-N
variable FILE-A
variable FILE-LEN
variable CUR-A
variable CUR-LEN
variable CUR-EXEMPT
variable ERR#

: FILE-A-FIELD ( -- ptr ptr u8 )
   FILE-A 0 ptr-field ;

: CUR-A-FIELD ( -- ptr ptr u8 )
   CUR-A 0 ptr-field ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-LEN @ ;

: SRC$ ( -- ptr u8 n )
   SRC-BUF SRC-LEN @ ;

: CUR$ ( -- ptr u8 n )
   CUR-A-FIELD @ CUR-LEN @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-result-cache" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-LEN !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" src.f" SRC-BUF JOIN-PATH SRC-LEN !
   ROOT$ s" content-key.cache" CACHE-BUF JOIN-PATH CACHE-LEN !
   CK-CACHE-CLEAR!
   CACHE-BUF CACHE-LEN @ CK-CACHE-PATH!
   FILE-CAP MEM-ALLOC-BYTES drop FILE-A-FIELD !
   TRC:RESET
   ROOT$ TRC:ROOT! ;

: KEY! ( ptr u8 -- ) {: dst:ptr :}
   CK-RESET
   s" result-cache-test" CK-TEXT+
   SRC$ CK-FILE+
   dst CK-FINAL-HEX ;

: STAMP-MISS-THEN-HIT ( -- )
   SRC$ s" alpha" WRITE-ALL
   KEY1 KEY!
   KEY1 TRC:HIT? TFALSE
   0 KEY1 TRC:PENDING+
   TRC:PENDING# 1 T=
   0 TRC:PENDING-PHASE 0 T=
   KEY1 TRC:HIT? TFALSE
   s" rct-phase" KEY1 TRC:STAMP+
   KEY1 TRC:HIT? TTRUE ;

: EDIT-INVALIDATES ( -- )
   SRC$ s" beta-longer" WRITE-ALL
   KEY2 KEY!
   KEY2 KEY-U KEY1 KEY-U T$<>
   KEY2 TRC:HIT? TFALSE ;

: RED-NEVER-CACHED ( -- )
   TRC:RESET
   TRC:PENDING# 0 T=
   8 KEY2 TRC:PENDING+
   TRC:PENDING# 1 T=
   0 TRC:PENDING-PHASE 8 T=
   KEY2 TRC:HIT? TFALSE ;

\ Closure lint: every declared phase file set must contain each existing
\ source file referenced by its members via require/include lines or s"
\ literals, so a stale set cannot produce a false PASS (cached). src/
\ members are keyed but not scanned; the scheduler files reference other
\ phases' files by design and are exempt from the s" scan only.

: SET-RESET ( -- )
   0 SET-LEN !
   0 SET-N ! ;

: SET+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   SET-LEN @ u + 1 + SET-CAP > if E-STR-CAPACITY throw then
   u SET-BUF SET-LEN @ + c!
   a SET-BUF SET-LEN @ + 1 + u BYTE-COPY
   SET-LEN @ u + 1 + SET-LEN !
   SET-N @ 1 + SET-N ! ;

: SET-ENTRY ( n -- ptr u8 n ) {: want:n :}
   0 want begin dup 0 > while
      swap dup SET-BUF + c@ + 1 + swap 1-
   repeat drop {: off:n :}
   SET-BUF off + 1 + SET-BUF off + c@ ;

: MEMBER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup SET-N @ < while
      dup SET-ENTRY a u STR= if drop 0 0= exit then
      1+
   repeat drop 0 0= 0= ;

: LOAD-CUR ( -- )
   CUR$ FILE-A-FIELD @ FILE-CAP READ-ALL FILE-LEN ! ;

: FILE-C@ ( n -- n ) {: i:n :}
   FILE-A-FIELD @ i + c@ ;

: FILE$ ( n n -- ptr u8 n ) {: i:n j:n :}
   FILE-A-FIELD @ i + j i - ;

: SPACE-C? ( n -- bool ) {: c:n :}
   c BL-C = if 0 0= exit then
   c TAB-C = if 0 0= exit then
   c STR-CR = ;

: SKIP-SPACES ( n n -- n ) {: i:n end:n :}
   i begin dup end < while
      dup FILE-C@ SPACE-C? 0= if exit then
      1+
   repeat ;

: TOKEN-END ( n n -- n ) {: i:n end:n :}
   i begin dup end < while
      dup FILE-C@ SPACE-C? if exit then
      1+
   repeat ;

: LINE-END ( n -- n )
   begin dup FILE-LEN @ < while
      dup FILE-C@ STR-LF = if exit then
      1+
   repeat ;

: SOURCE-REF? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" ENDS-WITH? if 0 0= exit then
   a u s" .fs" ENDS-WITH? ;

: REF-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SOURCE-REF? 0= if exit then
   a u EXISTS? 0= if exit then
   a u MEMBER? if exit then
   1 ERR# +!
   s" result-cache closure: " type
   CUR$ type
   s"  -> missing " type
   a u type cr ;

: REQUIRE-LINE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" require" STR= if 0 0= exit then
   a u s" include" STR= ;

: LINT-REQUIRE ( n n -- ) {: p:n end:n :}
   p end TOKEN-END {: q:n :}
   p q FILE$ REQUIRE-LINE? 0= if exit then
   q end SKIP-SPACES {: r:n :}
   r end >= if exit then
   r r end TOKEN-END FILE$ REF-CHECK ;

: SQUOTE-AT? ( n n -- bool ) {: i:n end:n :}
   i 2 + end >= if 0 0= 0= exit then
   i FILE-C@ dup $73 = swap $53 = or 0= if 0 0= 0= exit then
   i 1 + FILE-C@ DQ-C = 0= if 0 0= 0= exit then
   i 2 + FILE-C@ BL-C = ;

: SQUOTE-BOUNDARY? ( n n -- bool ) {: i:n off:n :}
   i off <= if 0 0= exit then
   i 1- FILE-C@ SPACE-C? ;

: SQUOTE-CLOSE ( n n -- n ) {: i:n end:n :}
   i begin dup end < while
      dup FILE-C@ DQ-C = if exit then
      1+
   repeat ;

: LINT-SQUOTE-AT ( n n -- ) {: i:n end:n :}
   i 3 + end SQUOTE-CLOSE {: j:n :}
   j end >= if exit then
   i 3 + j FILE$ REF-CHECK ;

: LINT-SQUOTES ( n n -- ) {: off:n end:n :}
   off begin dup end < while
      dup end SQUOTE-AT? if
         dup off SQUOTE-BOUNDARY? if
            dup end LINT-SQUOTE-AT
         then
      then
      1+
   repeat drop ;

: LINT-LINE ( n n -- ) {: off:n end:n :}
   off end SKIP-SPACES {: p:n :}
   p end >= if exit then
   p FILE-C@ $5C = if exit then
   p end LINT-REQUIRE
   CUR-EXEMPT @ 0= if off end LINT-SQUOTES then ;

: CUR-EXEMPT! ( -- )
   0 CUR-EXEMPT !
   CUR$ s" test/run-lib.f" STR= if -1 CUR-EXEMPT ! then
   CUR$ s" test/run-resident.f" STR= if -1 CUR-EXEMPT ! then
   CUR$ s" test/run-worker.f" STR= if -1 CUR-EXEMPT ! then
   CUR$ s" tools/dynamic-tail-manifest.f" STR= if -1 CUR-EXEMPT ! then ;

: LINT-CUR ( -- )
   CUR$ s" src/" STARTS-WITH? if exit then
   CUR-EXEMPT!
   LOAD-CUR
   0 begin dup FILE-LEN @ < while
      dup LINE-END {: end:n :}
      dup end LINT-LINE
      drop end 1 +
   repeat drop ;

: CUR! ( ptr u8 n -- ) {: a:ptr u:n :}
   a CUR-A-FIELD !
   u CUR-LEN ! ;

: LINT-SET ( -- )
   0 begin dup SET-N @ < while
      dup SET-ENTRY CUR!
      LINT-CUR
      1+
   repeat drop ;

: DEBUG-SET! ( -- )
   SET-RESET
   [: SET+ ;] TR-GATE-HARNESS-FILES
   [: SET+ ;] TR-GATE-COMMON-FILES
   [: SET+ ;] TR-DEBUG-PHASE-FILES ;

: AOT-NEG-SET! ( -- )
   SET-RESET
   [: SET+ ;] TR-GATE-HARNESS-FILES
   [: SET+ ;] TR-GATE-COMMON-FILES
   [: SET+ ;] TR-AOT-NEG-PHASE-FILES ;

: CLOSURE-LINT ( -- )
   0 ERR# !
   DEBUG-SET! LINT-SET
   AOT-NEG-SET! LINT-SET
   ERR# @ 0 T= ;

: CLEANUP ( -- )
   CK-CACHE-CLEAR!
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE ;

: MAIN ( -- )
   T-RESET
   SETUP
   STAMP-MISS-THEN-HIT
   EDIT-INVALIDATES
   RED-NEVER-CACHED
   CLOSURE-LINT
   CLEANUP
   T-REPORT ;

MAIN

;package

\ dot-dep-lint-core.f - validate dot blocker references.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ tools/lint/text.f, and tools/lint/intern.f.

$40000 constant DDL-FILE-CAP
32 constant DDL-NUM-CAP
10 constant DDL-LF
45 constant DDL-DASH
48 constant DDL-ZERO
58 constant DDL-COLON
3 constant DDL-MD-U

create DDL-FILE-BUF DDL-FILE-CAP allot
create DDL-NUM-BUF DDL-NUM-CAP allot

variable DDL-BAD
variable DDL-DOT#
variable DDL-BLOCK#
variable DDL-NUM-L
variable DDL-FM
variable DDL-BLOCKS
variable DDL-LINE

: DDL-NL ( -- )
   DDL-LF emit ;

: DDL-U. ( n -- )
   0 DDL-NUM-L !
   dup 0= IF drop DDL-ZERO emit exit THEN
   begin dup 0 > while
      dup 10 mod DDL-ZERO + DDL-NUM-BUF DDL-NUM-L @ + c!
      10 /
      DDL-NUM-L @ 1+ DDL-NUM-L !
   repeat drop
   begin DDL-NUM-L @ 0 > while
      DDL-NUM-L @ 1- DDL-NUM-L !
      DDL-NUM-BUF DDL-NUM-L @ + c@ emit
   repeat ;

: DDL-BAD+ ( -- )
   DDL-BAD @ 1+ DDL-BAD ! ;

: DDL-DOT+ ( -- )
   DDL-DOT# @ 1+ DDL-DOT# ! ;

: DDL-BLOCK+ ( -- )
   DDL-BLOCK# @ 1+ DDL-BLOCK# ! ;

: DDL-SKIP ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n n:n :}
   a n +  u n - ;

: DDL-DROP-TAIL ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n n:n :}
   n u > IF s" dot-dep-lint: short string" 1 die THEN
   a u n - ;

: DDL-MD? ( ptr u8 n -- bool )
   s" .md" ENDS-WITH? ;

: DDL-DOT-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .dots/" STARTS-WITH?
   a u DDL-MD? and ;

: DDL-DOT-ID$ ( ptr u8 n -- ptr u8 n )
   BASENAME DDL-MD-U DDL-DROP-TAIL ;

: DDL-FM-MARK? ( ptr u8 n -- bool )
   TRIM s" ---" STR= ;

: DDL-BLOCKS-LINE? ( ptr u8 n -- bool )
   TRIM s" blocks:" STR= ;

: DDL-BLOCKER-LINE-TRIM? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 < IF LINT-FALSE exit THEN
   a c@ DDL-DASH <> IF LINT-FALSE exit THEN
   a 1 + c@ WS? ;

: DDL-BLOCKER-LINE? ( ptr u8 n -- bool )
   TRIM DDL-BLOCKER-LINE-TRIM? ;

: DDL-BLOCKER$ ( ptr u8 n -- ptr u8 n )
   TRIM 2 DDL-SKIP TRIM ;

: DDL-FM-OPEN? ( -- bool )
   DDL-FM @ 1 = ;

: DDL-OPEN-FM ( -- )
   1 DDL-FM !
   0 DDL-BLOCKS ! ;

: DDL-CLOSE-FM ( -- )
   2 DDL-FM !
   0 DDL-BLOCKS ! ;

: DDL-SEE-FM-MARK ( -- )
   DDL-FM @ 0= IF DDL-OPEN-FM exit THEN
   DDL-FM @ 1 = IF DDL-CLOSE-FM exit THEN ;

: DDL-NONEMPTY? ( ptr u8 n -- bool )
   TRIM nip 0 > ;

: DDL-MISSING ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n line:n id:ptr idu:n :}
   s" DOT-DEP-MISSING " type
   path pathu type
   DDL-COLON emit
   line DDL-U.
   s" : missing blocker " type
   id idu type
   DDL-NL
   DDL-BAD+ ;

: DDL-CHECK-BLOCKER ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n line:n id:ptr idu:n :}
   DDL-BLOCK+
   id idu INTERN? 0= IF path pathu line id idu DDL-MISSING THEN ;

: DDL-SCAN-BLOCK-LINE ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n path:ptr pathu:n line:n :}
   a u DDL-BLOCKER-LINE? IF
      path pathu line a u DDL-BLOCKER$ DDL-CHECK-BLOCKER
      exit
   THEN
   a u DDL-NONEMPTY? IF 0 DDL-BLOCKS ! THEN ;

: DDL-SCAN-FM-LINE ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n path:ptr pathu:n line:n :}
   a u DDL-BLOCKS-LINE? IF 1 DDL-BLOCKS ! exit THEN
   DDL-BLOCKS @ 0= IF exit THEN
   a u path pathu line DDL-SCAN-BLOCK-LINE ;

: DDL-SCAN-LINE ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n path:ptr pathu:n line:n :}
   a u DDL-FM-MARK? IF DDL-SEE-FM-MARK exit THEN
   DDL-FM-OPEN? 0= IF exit THEN
   a u path pathu line DDL-SCAN-FM-LINE ;

: DDL-COLLECT-DOT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu DDL-DOT-PATH? 0= IF exit THEN
   path pathu DDL-DOT-ID$ INTERN drop
   DDL-DOT+ ;

: DDL-SCAN-DOT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu DDL-DOT-PATH? 0= IF exit THEN
   0 DDL-FM !
   0 DDL-BLOCKS !
   path pathu DDL-FILE-BUF DDL-FILE-CAP READ-FILE SPLIT-LINES
   0 begin dup SN# @ < while
      dup 1+ DDL-LINE !
      dup S@ path pathu DDL-LINE @ DDL-SCAN-LINE
      1+
   repeat drop ;

: DOT-DEP-LINT ( -- )
   0 DDL-BAD !
   0 DDL-DOT# !
   0 DDL-BLOCK# !
   INTERN-RESET
   s" .dots/" [: DDL-COLLECT-DOT ;] WALK-FILES
   s" .dots/" [: DDL-SCAN-DOT ;] WALK-FILES
   s" dot-dep-lint: " type
   DDL-DOT# @ DDL-U.
   s"  dot(s), " type
   DDL-BLOCK# @ DDL-U.
   s"  blocker(s), " type
   DDL-BAD @ DDL-U.
   s"  finding(s)" type DDL-NL
   DDL-BAD @ 0 > IF 1 throw THEN ;

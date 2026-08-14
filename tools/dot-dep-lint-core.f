\ dot-dep-lint-core.f - validate dot blocker references.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ lib/fs-mutate.f, tools/lint/text.f, and tools/lint/intern.f.

require lib/sort.f

package DOT-DEP
using LINT-SPLIT

$40000 constant DDP-FILE-CAP
32 constant DDP-NUM-CAP
10 constant DDP-LF
45 constant DDP-DASH
48 constant DDP-ZERO
58 constant DDP-COLON
3 constant DDP-MD-U

create DDP-FILE-BUF DDP-FILE-CAP allot
create DDP-NUM-BUF DDP-NUM-CAP allot
create DDP-CHAR-BUF 1 allot
create PATH-BUF DDP-FILE-CAP allot

create ROOT-BUF FS-PATH-CAP allot
create IDS VEC-HEADER-CELLS cells allot
create ROW-IDS VEC-HEADER-CELLS cells allot
create ROW-PATH-OFFSETS VEC-HEADER-CELLS cells allot
create ROW-PATH-LENGTHS VEC-HEADER-CELLS cells allot
create ROW-ORDER VEC-HEADER-CELLS cells allot

variable DDP-BAD
variable DDP-DOT#
variable DDP-BLOCK#
variable DDP-NUM-L
variable DDP-FM
variable DDP-BLOCKS
variable DDP-LINE
variable ROOT-U
variable INDEX-READY
variable CMP-I
variable PATH-FIRST
variable PATH-U

: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;

: OUT-C ( n -- )
   DDP-CHAR-BUF c!
   DDP-CHAR-BUF 1 OUT ;

: DDP-NL ( -- )
   DDP-LF OUT-C ;

: DDP-U$ ( n -- ptr u8 n )
   0 DDP-NUM-L !
   dup 0= IF
      drop DDP-ZERO DDP-NUM-BUF c!
      DDP-NUM-BUF 1 exit
   THEN
   begin dup 0 > while
      dup 10 mod DDP-ZERO + DDP-NUM-BUF DDP-NUM-L @ + c!
      10 /
      DDP-NUM-L @ 1+ DDP-NUM-L !
   repeat drop
   DDP-NUM-L @ 2 / 0 ?do
      DDP-NUM-BUF i + c@
      DDP-NUM-BUF DDP-NUM-L @ 1- i - + c@
      DDP-NUM-BUF i + c!
      DDP-NUM-BUF DDP-NUM-L @ 1- i - + c!
   loop
   DDP-NUM-BUF DDP-NUM-L @ ;

: DDP-BAD+ ( -- )
   DDP-BAD @ 1+ DDP-BAD ! ;

: DDP-DOT+ ( -- )
   DDP-DOT# @ 1+ DDP-DOT# ! ;

: DDP-BLOCK+ ( -- )
   DDP-BLOCK# @ 1+ DDP-BLOCK# ! ;

: DDP-SKIP ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n n:n :}
   a n +  u n - ;

: DDP-DROP-TAIL ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n n:n :}
   n u > IF s" dot-dep-lint: short string" 1 die THEN
   a u n - ;

: DDP-MD? ( ptr u8 n -- bool )
   s" .md" LINT-ENDS-WITH? ;

: DDP-DOT-ID$ ( ptr u8 n -- ptr u8 n )
   BASENAME DDP-MD-U DDP-DROP-TAIL ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= IF E-FS-PATH throw THEN
   u FS-PATH-CAP > IF E-FS-PATH throw THEN
   a ROOT-BUF u BYTE-COPY
   u 0 > IF a u 1- + c@ 47 = IF u ROOT-U ! exit THEN THEN
   u 1+ FS-PATH-CAP > IF E-FS-PATH throw THEN
   47 ROOT-BUF u + c!
   u 1+ ROOT-U ! ;

: ROOT-ARCHIVE-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u ROOT$ LINT-STARTS-WITH? 0= IF LINT-FALSE exit THEN
   a u ROOT-U @ DDP-SKIP s" archive/" LINT-STARTS-WITH? ;

: ROOT-DOT-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u ROOT$ LINT-STARTS-WITH? 0= IF LINT-FALSE exit THEN
   a u ROOT-ARCHIVE-PATH? IF LINT-FALSE exit THEN
   a u DDP-MD? ;

: INDEX-INIT ( -- )
   INDEX-READY @ IF exit THEN
   IDS 8 >COUNT VEC-INIT
   ROW-IDS 8 >COUNT VEC-INIT
   ROW-PATH-OFFSETS 8 >COUNT VEC-INIT
   ROW-PATH-LENGTHS 8 >COUNT VEC-INIT
   ROW-ORDER 8 >COUNT VEC-INIT
   1 INDEX-READY ! ;

: INDEX-RESET ( -- )
   INDEX-INIT
   IDS VEC-CLEAR
   ROW-IDS VEC-CLEAR
   ROW-PATH-OFFSETS VEC-CLEAR
   ROW-PATH-LENGTHS VEC-CLEAR
   ROW-ORDER VEC-CLEAR
   0 PATH-U ! ;

: INTERN-DOT-ID ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u INTERN-FIND dup 0 >= IF exit THEN drop
   a u INTERN dup IDS VEC-PUSH-N drop ;

: STORE-PATH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PATH-U @ pathu + DDP-FILE-CAP > IF
      s" dot-dep-lint: path index full" E-FS-CAPACITY die
   THEN
   path PATH-BUF PATH-U @ + pathu BYTE-COPY
   PATH-U @ ROW-PATH-OFFSETS VEC-PUSH-N drop
   pathu ROW-PATH-LENGTHS VEC-PUSH-N drop
   PATH-U @ pathu + PATH-U ! ;

: INDEX-DOT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu DDP-DOT-ID$ INTERN-DOT-ID ROW-IDS VEC-PUSH-N drop
   path pathu STORE-PATH
   DDP-DOT+ ;

: TEXT< ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   0 CMP-I !
   begin CMP-I @ u < CMP-I @ v < and while
      a CMP-I @ + c@ b CMP-I @ + c@
      2dup < IF 2drop LINT-TRUE exit THEN
      > IF LINT-FALSE exit THEN
      CMP-I @ 1+ CMP-I !
   repeat
   u v < ;

: ID-BEFORE? ( n n -- bool ) {: left:n right:n :}
   left INTERN$ right INTERN$ TEXT< ;

: ROW-PATH$ ( n -- ptr u8 n )
   >IDX dup ROW-PATH-OFFSETS swap VEC-N@ PATH-BUF +
   swap ROW-PATH-LENGTHS swap VEC-N@ ;

: ROW-BEFORE? ( n n -- bool ) {: left:n right:n :}
   left ROW-PATH$ right ROW-PATH$ TEXT< ;

: PREPARE-ORDER ( -- )
   ROW-ORDER VEC-CLEAR
   ROW-IDS VEC-LEN@ LEN>N 0 ?do i ROW-ORDER VEC-PUSH-N drop loop
   IDS VEC-DATA@ IDS VEC-LEN@ LEN>N [: ID-BEFORE? ;] SORT:SORT!
   ROW-ORDER VEC-DATA@ ROW-ORDER VEC-LEN@ LEN>N [: ROW-BEFORE? ;] SORT:SORT! ;

: ID-COUNT ( n -- n ) {: id:n :}
   0
   ROW-IDS VEC-LEN@ LEN>N 0 ?do
      ROW-IDS i >IDX VEC-N@ id = IF 1+ THEN
   loop ;

: REPORT-PATH ( n n -- ) {: row:n id:n :}
   ROW-IDS row >IDX VEC-N@ id <> IF exit THEN
   PATH-FIRST @ 0= IF s" , " OUT THEN
   row ROW-PATH$ OUT
   0 PATH-FIRST ! ;

: REPORT-DUPLICATE ( n -- ) {: id:n :}
   id ID-COUNT 2 < IF exit THEN
   s" DOT-DEP-DUPLICATE " OUT
   id INTERN$ OUT
   s" : " OUT
   1 PATH-FIRST !
   ROW-ORDER VEC-LEN@ LEN>N 0 ?do
      ROW-ORDER i >IDX VEC-N@ id REPORT-PATH
   loop
   DDP-NL
   DDP-BAD+ ;

: REPORT-DUPLICATES ( -- )
   PREPARE-ORDER
   IDS VEC-LEN@ LEN>N 0 ?do
      IDS i >IDX VEC-N@ REPORT-DUPLICATE
   loop ;

: DDP-FM-MARK? ( ptr u8 n -- bool )
   LINT-TRIM s" ---" LINT-STR= ;

: DDP-BLOCKS-LINE? ( ptr u8 n -- bool )
   LINT-TRIM s" blocks:" LINT-STR= ;

: DDP-BLOCKER-LINE-TRIM? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 < IF LINT-FALSE exit THEN
   a c@ DDP-DASH <> IF LINT-FALSE exit THEN
   a 1 + c@ LINT-WS? ;

: DDP-BLOCKER-LINE? ( ptr u8 n -- bool )
   LINT-TRIM DDP-BLOCKER-LINE-TRIM? ;

: DDP-BLOCKER$ ( ptr u8 n -- ptr u8 n )
   LINT-TRIM 2 DDP-SKIP LINT-TRIM ;

: DDP-FM-OPEN? ( -- bool )
   DDP-FM @ 1 = ;

: DDP-OPEN-FM ( -- )
   1 DDP-FM !
   0 DDP-BLOCKS ! ;

: DDP-CLOSE-FM ( -- )
   2 DDP-FM !
   0 DDP-BLOCKS ! ;

: DDP-SEE-FM-MARK ( -- )
   DDP-FM @ 0= IF DDP-OPEN-FM exit THEN
   DDP-FM @ 1 = IF DDP-CLOSE-FM exit THEN ;

: DDP-NONEMPTY? ( ptr u8 n -- bool )
   LINT-TRIM nip 0 > ;

: DDP-MISSING ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n line:n id:ptr idu:n :}
   s" DOT-DEP-MISSING " OUT
   path pathu OUT
   DDP-COLON OUT-C
   line DDP-U$ OUT
   s" : missing blocker " OUT
   id idu OUT
   DDP-NL
   DDP-BAD+ ;

: DDP-CHECK-BLOCKER ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n line:n id:ptr idu:n :}
   DDP-BLOCK+
   id idu INTERN? 0= IF path pathu line id idu DDP-MISSING THEN ;

: DDP-SCAN-BLOCK-LINE ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n path:ptr pathu:n line:n :}
   a u DDP-BLOCKER-LINE? IF
      path pathu line a u DDP-BLOCKER$ DDP-CHECK-BLOCKER
      exit
   THEN
   a u DDP-NONEMPTY? IF 0 DDP-BLOCKS ! THEN ;

: DDP-SCAN-FM-LINE ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n path:ptr pathu:n line:n :}
   a u DDP-BLOCKS-LINE? IF 1 DDP-BLOCKS ! exit THEN
   DDP-BLOCKS @ 0= IF exit THEN
   a u path pathu line DDP-SCAN-BLOCK-LINE ;

: DDP-SCAN-LINE ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n path:ptr pathu:n line:n :}
   a u DDP-FM-MARK? IF DDP-SEE-FM-MARK exit THEN
   DDP-FM-OPEN? 0= IF exit THEN
   a u path pathu line DDP-SCAN-FM-LINE ;

: DDP-COLLECT-DOT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu ROOT-DOT-PATH? 0= IF exit THEN
   path pathu INDEX-DOT ;

: DDP-SCAN-DOT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu ROOT-DOT-PATH? 0= IF exit THEN
   0 DDP-FM !
   0 DDP-BLOCKS !
   path pathu DDP-FILE-BUF DDP-FILE-CAP READ-FILE SPLIT-LINES
   0 begin dup SN# @ < while
      dup 1+ DDP-LINE !
      dup S@ path pathu DDP-LINE @ DDP-SCAN-LINE
      1+
   repeat drop ;

: RESET ( ptr u8 n -- )
   ROOT!
   0 DDP-BAD !
   0 DDP-DOT# !
   0 DDP-BLOCK# !
   INTERN-RESET
   INDEX-RESET ;

: SUMMARY ( -- )
   s" dot-dep-lint: " OUT
   DDP-DOT# @ DDP-U$ OUT
   s"  dot(s), " OUT
   DDP-BLOCK# @ DDP-U$ OUT
   s"  blocker(s), " OUT
   DDP-BAD @ DDP-U$ OUT
   s"  finding(s)" OUT DDP-NL ;

public

: LINT-ROOT ( ptr u8 n -- )
   RESET
   ROOT$ [: DDP-COLLECT-DOT ;] WALK-FILES
   REPORT-DUPLICATES
   ROOT$ [: DDP-SCAN-DOT ;] WALK-FILES
   SUMMARY
   DDP-BAD @ 0 > IF 1 throw THEN ;

: LINT ( -- )
   s" .dots/" LINT-ROOT ;

;package

: DOT-DEP-LINT ( -- )
   DOT-DEP:LINT ;

\ dot-dep-lint-core.f - validate dot blocker references.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ lib/fs-mutate.f, tools/lint/text.f, and tools/lint/intern.f.

require lib/sort.f

package DOT-DEP

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
create IDS VEC:HEADER-CELLS cells allot
create ROW-IDS VEC:HEADER-CELLS cells allot
create ROW-PATH-OFFSETS VEC:HEADER-CELLS cells allot
create ROW-PATH-LENGTHS VEC:HEADER-CELLS cells allot
create ROW-ORDER VEC:HEADER-CELLS cells allot

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

: DDP>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;

: DDP>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

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

: INDEX-INIT1 ( ptr h n -- ) {: vec:ptr step:n :}
   step INDEX-READY @ < IF exit THEN
   vec 8 DDP>ITEM VEC:INIT
   step 1+ INDEX-READY ! ;

: INDEX-INIT ( -- )
   INDEX-READY @ 5 = IF exit THEN
   IDS              0 INDEX-INIT1
   ROW-IDS          1 INDEX-INIT1
   ROW-PATH-OFFSETS 2 INDEX-INIT1
   ROW-PATH-LENGTHS 3 INDEX-INIT1
   ROW-ORDER        4 INDEX-INIT1 ;

: INDEX-RESET ( -- )
   INDEX-INIT
   IDS VEC:CLEAR
   ROW-IDS VEC:CLEAR
   ROW-PATH-OFFSETS VEC:CLEAR
   ROW-PATH-LENGTHS VEC:CLEAR
   ROW-ORDER VEC:CLEAR
   0 PATH-U ! ;

: INTERN-DOT-ID ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LINT-INTERN:FIND dup 0 >= IF exit THEN drop
   IDS LINT-INTERN:COUNT 1+ DDP>ITEM VEC:ENSURE
   a u LINT-INTERN:ADD dup IDS VEC:PUSH drop ;

: RESERVE-ROW ( n -- ) {: pathu:n :}
   PATH-U @ pathu + DDP-FILE-CAP > IF
      s" dot-dep-lint: path index full" E-FS-CAPACITY die
   THEN
   DDP-DOT# @ 1+ DDP>ITEM {: need:CAD-NUM:item-count :}
   ROW-IDS          need VEC:ENSURE
   ROW-PATH-OFFSETS need VEC:ENSURE
   ROW-PATH-LENGTHS need VEC:ENSURE ;

: STORE-PATH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path PATH-BUF PATH-U @ + pathu BYTE-COPY
   PATH-U @ ROW-PATH-OFFSETS VEC:PUSH drop
   pathu ROW-PATH-LENGTHS VEC:PUSH drop
   PATH-U @ pathu + PATH-U ! ;

: INDEX-DOT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   pathu RESERVE-ROW
   path pathu DDP-DOT-ID$ INTERN-DOT-ID ROW-IDS VEC:PUSH drop
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
   left LINT-INTERN:TEXT right LINT-INTERN:TEXT TEXT< ;

: ROW-PATH$ ( n -- ptr u8 n )
   DDP>INDEX dup ROW-PATH-OFFSETS swap VEC:@ PATH-BUF +
   swap ROW-PATH-LENGTHS swap VEC:@ ;

: ROW-BEFORE? ( n n -- bool ) {: left:n right:n :}
   left ROW-PATH$ right ROW-PATH$ TEXT< ;

: PREPARE-ORDER ( -- )
   ROW-ORDER VEC:CLEAR
   ROW-ORDER DDP-DOT# @ DDP>ITEM VEC:ENSURE
   DDP-DOT# @ 0 ?do i ROW-ORDER VEC:PUSH drop loop
   IDS VEC:DATA@ LINT-INTERN:COUNT [: ID-BEFORE? ;] SORT:SORT!
   ROW-ORDER VEC:DATA@ DDP-DOT# @ [: ROW-BEFORE? ;] SORT:SORT! ;

: ID-COUNT ( n -- n ) {: id:n :}
   0
   DDP-DOT# @ 0 ?do
      ROW-IDS i DDP>INDEX VEC:@ id = IF 1+ THEN
   loop ;

: REPORT-PATH ( n n -- ) {: row:n id:n :}
   ROW-IDS row DDP>INDEX VEC:@ id <> IF exit THEN
   PATH-FIRST @ 0= IF s" , " OUT THEN
   row ROW-PATH$ OUT
   0 PATH-FIRST ! ;

: REPORT-DUPLICATE ( n -- ) {: id:n :}
   id ID-COUNT 2 < IF exit THEN
   s" DOT-DEP-DUPLICATE " OUT
   id LINT-INTERN:TEXT OUT
   s" : " OUT
   1 PATH-FIRST !
   DDP-DOT# @ 0 ?do
      ROW-ORDER i DDP>INDEX VEC:@ id REPORT-PATH
   loop
   DDP-NL
   DDP-BAD+ ;

: REPORT-DUPLICATES ( -- )
   PREPARE-ORDER
   LINT-INTERN:COUNT 0 ?do
      IDS i DDP>INDEX VEC:@ REPORT-DUPLICATE
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
   id idu LINT-INTERN:HAS? 0= IF path pathu line id idu DDP-MISSING THEN ;

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
   LINT-INTERN:RESET
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

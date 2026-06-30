\ dot-dep-lint-core.f - validate dot blocker references.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ tools/lint/text.f, and tools/lint/intern.f.

$40000 constant DDP-FILE-CAP
32 constant DDP-NUM-CAP
10 constant DDP-LF
45 constant DDP-DASH
48 constant DDP-ZERO
58 constant DDP-COLON
3 constant DDP-MD-U

create DDP-FILE-BUF DDP-FILE-CAP allot
create DDP-NUM-BUF DDP-NUM-CAP allot

variable DDP-BAD
variable DDP-DOT#
variable DDP-BLOCK#
variable DDP-NUM-L
variable DDP-FM
variable DDP-BLOCKS
variable DDP-LINE

: DDP-NL ( -- )
   DDP-LF emit ;

: DDP-U. ( n -- )
   0 DDP-NUM-L !
   dup 0= IF drop DDP-ZERO emit exit THEN
   begin dup 0 > while
      dup 10 mod DDP-ZERO + DDP-NUM-BUF DDP-NUM-L @ + c!
      10 /
      DDP-NUM-L @ 1+ DDP-NUM-L !
   repeat drop
   begin DDP-NUM-L @ 0 > while
      DDP-NUM-L @ 1- DDP-NUM-L !
      DDP-NUM-BUF DDP-NUM-L @ + c@ emit
   repeat ;

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

: DDP-ARCHIVE-PATH? ( ptr u8 n -- bool )
   s" .dots/archive/" LINT-STARTS-WITH? ;

: DDP-DOT-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u DDP-ARCHIVE-PATH? IF LINT-FALSE exit THEN
   a u s" .dots/" LINT-STARTS-WITH?
   a u DDP-MD? and ;

: DDP-DOT-ID$ ( ptr u8 n -- ptr u8 n )
   BASENAME DDP-MD-U DDP-DROP-TAIL ;

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
   s" DOT-DEP-MISSING " type
   path pathu type
   DDP-COLON emit
   line DDP-U.
   s" : missing blocker " type
   id idu type
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
   path pathu DDP-DOT-PATH? 0= IF exit THEN
   path pathu DDP-DOT-ID$ INTERN drop
   DDP-DOT+ ;

: DDP-SCAN-DOT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu DDP-DOT-PATH? 0= IF exit THEN
   0 DDP-FM !
   0 DDP-BLOCKS !
   path pathu DDP-FILE-BUF DDP-FILE-CAP READ-FILE SPLIT-LINES
   0 begin dup SN# @ < while
      dup 1+ DDP-LINE !
      dup S@ path pathu DDP-LINE @ DDP-SCAN-LINE
      1+
   repeat drop ;

: DOT-DEP-LINT ( -- )
   0 DDP-BAD !
   0 DDP-DOT# !
   0 DDP-BLOCK# !
   INTERN-RESET
   s" .dots/" [: DDP-COLLECT-DOT ;] WALK-FILES
   s" .dots/" [: DDP-SCAN-DOT ;] WALK-FILES
   s" dot-dep-lint: " type
   DDP-DOT# @ DDP-U.
   s"  dot(s), " type
   DDP-BLOCK# @ DDP-U.
   s"  blocker(s), " type
   DDP-BAD @ DDP-U.
   s"  finding(s)" type DDP-NL
   DDP-BAD @ 0 > IF 1 throw THEN ;

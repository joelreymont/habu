\ typed-local-diff-lint-core.f - reject newly added bare locals.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f,
\ lib/fs.f, tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/source-lex.f, and tools/lint/diff-frame.f.

require lib/adt/option.f
require tools/lint/diff-frame.f

package TYPED-LOCAL-DIFF
private

32 constant NUM-CAP
10 constant LF-C
58 constant COLON-C

create NUM NUM-CAP allot
create ONE 1 allot

PTR-VARIABLE DIFF-A
variable DIFF-CAP
PTR-VARIABLE FILE-A
variable FILE-U
variable DIFF-U
variable BAD#
variable NEW-LINE
variable IN-LOCALS
variable ALLOW-GROUP
variable NUM-I

: FILE-A@ ( -- ptr u8 )
   FILE-A @ ;

: DIFF-A@ ( -- ptr u8 )
   DIFF-A @ ;

: FILE-A! ( ptr u8 -- )
   FILE-A ! ;

: DIFF-A! ( ptr u8 -- )
   DIFF-A ! ;

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: NOT ( bool -- bool )
   if FALSE else TRUE then ;

: FILE$ ( -- ptr u8 n )
   FILE-A@ FILE-U @ ;

: U$ ( n -- ptr u8 n ) {: u:n :}
   NUM-CAP NUM-I !
   u 0= if
      NUM-I @ 1- NUM-I !
      48 NUM NUM-I @ + c!
      NUM NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      NUM-I @ 1- NUM-I !
      NUM NUM-I @ + c!
      10 /
   repeat drop
   NUM NUM-I @ + NUM-CAP NUM-I @ - ;

: WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   fd a u LINT-OUT-WRITE ;

: OUT ( ptr u8 n -- )
   1 -rot WRITE ;

: C ( n -- ) {: c:n :}
   c ONE c!
   ONE 1 OUT ;

: BAD+ ( -- )
   BAD# @ 1+ BAD# ! ;

: TOKEN= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LEX-TOK a u LINT-STR= ;

: TYPED-LOCAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u COLON-C LINT-INDEX-OF MATCH option
      none OF FALSE ENDOF
      some OF drop TRUE ENDOF
   ;MATCH ;

: FORTH-FILE? ( -- bool )
   FILE$ s" .f" LINT-ENDS-WITH? if TRUE exit then
   FILE$ s" .fs" LINT-ENDS-WITH? ;

: ALLOW-LINE? ( ptr u8 n -- bool )
   s" typed-local-lint: allow-bare-local" LINT-CONTAINS? ;

: SOURCE-LINE ( n -- n ) {: k:n :}
   NEW-LINE @ k LL@ + 1- ;

: REPORT-LOCAL ( n -- ) {: k:n :}
   BAD+
   s" E-UNTYPED-LOCAL " OUT
   FILE$ dup 0= if 2drop s" <unknown>" then OUT
   COLON-C C
   k SOURCE-LINE U$ OUT
   COLON-C C
   k LC@ U$ OUT
   s" : `" OUT
   k LEX-TOK OUT
   s" ` needs :type inside {: :}" OUT
   LF-C C ;

: SCAN-LOCAL-TOKEN ( n -- ) {: k:n :}
   k s" {:" TOKEN= if TRUE IN-LOCALS ! exit then
   k s" :}" TOKEN= if
      FALSE IN-LOCALS !
      FALSE ALLOW-GROUP !
      exit
   then
   IN-LOCALS @ NOT if exit then
   ALLOW-GROUP @ if exit then
   k LEX-TOK TYPED-LOCAL? NOT if k REPORT-LOCAL then ;

: SCAN-ADDED-SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   FORTH-FILE? NOT if exit then
   a u ALLOW-LINE? if TRUE ALLOW-GROUP ! then
   a u LEX-SOURCE
   0 begin dup L# @ < while
      dup SCAN-LOCAL-TOKEN
      1+
   repeat drop ;

: INC-LINE ( -- )
   NEW-LINE @ 1+ NEW-LINE ! ;

: SCAN-ADDED-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SCAN-ADDED-SOURCE
   INC-LINE ;

: RESET-FILE ( -- )
   FALSE IN-LOCALS !
   FALSE ALLOW-GROUP ! ;

: SET-FILE$ ( ptr u8 n -- ) {: a:ptr u:n :}
   a FILE-A! u FILE-U !
   RESET-FILE ;

: START-HUNK ( n -- )
   NEW-LINE !
   FALSE IN-LOCALS !
   FALSE ALLOW-GROUP ! ;

: DROP-EVENT ( ptr u8 n n -- )
   drop 2drop ;

: FILE-EVENT ( ptr u8 n n -- )
   drop SET-FILE$ ;

: HUNK-EVENT ( ptr u8 n n -- )
   nip nip START-HUNK ;

: ADD-EVENT ( ptr u8 n n -- )
   drop SCAN-ADDED-LINE ;

: CONTEXT-EVENT ( ptr u8 n n -- )
   DROP-EVENT INC-LINE ;

: PROCESS-EVENT ( ptr u8 n n DIFF:event -- )
   MATCH DIFF:event
      none    OF DROP-EVENT ENDOF
      file    OF FILE-EVENT ENDOF
      hunk    OF HUNK-EVENT ENDOF
      add     OF ADD-EVENT ENDOF
      context OF CONTEXT-EVENT ENDOF
      delete  OF DROP-EVENT ENDOF
   ;MATCH ;

: NEXT? ( -- bool )
   DIFF:NEXT? {: a:ptr u:n value:n kind:DIFF:event present:bool :}
   present 0= if FALSE exit then
   a u value kind PROCESS-EVENT
   TRUE ;

: ALLOC-DIFF ( n -- ) {: need:n :}
   need 1 < if 1 else need then
   MEM-ALLOC-64K-SPAN DIFF-CAP ! DIFF-A! ;

public

: RESET ( -- )
   0 BAD# !
   0 DIFF-U !
   0 FILE-U !
   0 NEW-LINE !
   FALSE IN-LOCALS !
   FALSE ALLOW-GROUP ! ;

: SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIFF:OPEN
   begin NEXT? while repeat ;

: FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE ALLOC-DIFF
   path pathu DIFF-A@ DIFF-CAP @ READ-ALL DIFF-U !
   DIFF-A@ DIFF-U @ SOURCE ;

: FINISH ( -- )
   BAD# @ 0 > if 1 throw then ;

;package

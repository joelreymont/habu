\ typed-local-diff-lint-core.f - reject newly added bare locals.
\ Load after the lint, lexer, memory, and filesystem libraries.

require lib/adt/option.f
require tools/lint/text.f
require tools/lint/diff.f

package TYPED-LOCAL-DIFF
private

32 constant NUM-CAP
10 constant LF-C
58 constant COLON-C

create NUM NUM-CAP allot
create ONE 1 allot

variable FILE-A
variable FILE-U
variable BAD
variable NEW-LINE
variable IN-LOCALS
variable ALLOW-GROUP
variable NUM-I
variable SCAN-START

: PTR-SLOT ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: FILE-PTR ( -- ptr ptr u8 )
   FILE-A PTR-SLOT ;

: FILE$ ( -- ptr u8 n )
   FILE-PTR @ FILE-U @ ;

: U$ ( n -- ptr u8 n ) {: u:n :}
   NUM-CAP NUM-I !
   u 0= if
      NUM-I @ 1- NUM-I !
      48 NUM NUM-I @ + c!
      NUM NUM-I @ + 1 exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      NUM-I @ 1- NUM-I !
      NUM NUM-I @ + c!
      10 /
   repeat drop
   NUM NUM-I @ + NUM-CAP NUM-I @ - ;

: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;

: EMIT-C ( n -- ) {: c:n :}
   c ONE c!
   ONE 1 OUT ;

: BAD+ ( -- )
   BAD @ 1+ BAD ! ;

: TOKEN= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LEX-TOK a u LINT-STR= ;

: TYPED? ( ptr u8 n -- bool )
   COLON-C LINT-INDEX-OF MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: FORTH? ( -- bool )
   FILE$ s" .f" LINT-ENDS-WITH? if true exit then
   FILE$ s" .fs" LINT-ENDS-WITH? ;

: ALLOW? ( ptr u8 n -- bool )
   s" typed-local-lint: allow-bare-local" LINT-CONTAINS? ;

: SOURCE-LINE ( n -- n )
   LL@ NEW-LINE @ + 1- ;

: REPORT ( n -- ) {: k:n :}
   BAD+
   s" E-UNTYPED-LOCAL " OUT
   FILE$ dup 0= if 2drop s" <unknown>" then OUT
   COLON-C EMIT-C k SOURCE-LINE U$ OUT
   COLON-C EMIT-C k LC@ U$ OUT
   s" : `" OUT k LEX-TOK OUT
   s" ` needs :type inside {: :}" OUT
   LF-C EMIT-C ;

: LOCAL ( n -- ) {: k:n :}
   k s" {:" TOKEN= if true IN-LOCALS ! exit then
   k s" :}" TOKEN= if
      false IN-LOCALS ! false ALLOW-GROUP ! exit
   then
   IN-LOCALS @ 0= if exit then
   ALLOW-GROUP @ if exit then
   k LEX-TOK TYPED? 0= if k REPORT then ;

: SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   FORTH? 0= if exit then
   a u ALLOW? if true ALLOW-GROUP ! then
   a u LEX-SOURCE
   0 begin dup L# @ < while
      dup LOCAL 1+
   repeat drop ;

: LINE+ ( -- )
   NEW-LINE @ 1+ NEW-LINE ! ;

: ADD-LINE ( ptr u8 n -- )
   SCAN LINE+ ;

: FILE-RESET ( -- )
   false IN-LOCALS !
   false ALLOW-GROUP ! ;

: SECTION ( ptr u8 n ptr u8 n DIFF:form bool -- )
   {: oa:ptr ou:n na:ptr nu:n kind:DIFF:form body:bool :}
   oa ou 2drop kind drop body drop
   na FILE-PTR ! nu FILE-U !
   FILE-RESET ;

: HUNK ( n -- )
   NEW-LINE ! FILE-RESET ;

: EVENT ( DIFF:event -- )
   MATCH DIFF:event
      none OF ENDOF
      section OF
         DIFF:SECTION-OLD$ DIFF:SECTION-NEW$
         DIFF:SECTION-FORM DIFF:SECTION-BODY? SECTION
      ENDOF
      hunk OF DIFF:HUNK-NEW-START HUNK ENDOF
      add OF DIFF:CONTENT$ ADD-LINE ENDOF
      context OF LINE+ ENDOF
      delete OF ENDOF
   ;MATCH ;

: LINE ( ptr u8 n -- )
   DIFF:LINE EVENT ;

: SOURCE-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIFF:SOURCE-VALIDATE
   0 SCAN-START !
   0 begin dup u < while
      dup a + c@ LF-C = if
         a SCAN-START @ + over SCAN-START @ - LINE
         dup 1+ SCAN-START !
      then
      1+
   repeat drop ;

: END-ARTIFACT ( -- )
   DIFF:FINISH EVENT
   DIFF:RESET ;

public

: RESET ( -- )
   DIFF:RESET
   0 BAD ! 0 FILE-U ! 0 NEW-LINE !
   FILE-RESET ;

: SOURCE ( ptr u8 n -- )
   SOURCE-LINES ;

: FILE ( ptr u8 n -- )
   LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT SOURCE
   END-ARTIFACT ;

: FINISH ( -- )
   DIFF:FINISH EVENT
   BAD @ 0 > if 1 throw then ;

;package

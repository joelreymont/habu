\ kernel-perf-lint-core.f - require evidence for kernel-codegen changes.
\ Load after the lint, filesystem, and profile-registry libraries.

require tools/lint/diff.f

package KERNEL-PERF-LINT
private

16 constant WATCH-MAX
10 constant LF-C

create ONE 1 allot
create WATCH-PATHS WATCH-MAX FS-PATH-CAP * allot
create WATCH-US WATCH-MAX cells allot

variable BAD
variable ROWS+
variable CUR-REG
variable WATCH#
variable SCAN-START

: REG-PATH$ ( -- ptr u8 n )
   s" tools/ptx/perf-rows.tsv" ;

: OUT ( ptr u8 n -- )
   1 -rot LINT-OUT-WRITE ;

: EMIT-C ( n -- ) {: c:n :}
   c ONE c!
   ONE 1 OUT ;

: BAD+ ( -- )
   BAD @ 1+ BAD ! ;

: WATCHED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" src/arch/ptx/emit.f" LINT-STR= if true exit then
   a u s" lib/ptx/cg" LINT-STARTS-WITH?
   a u s" .f" HAS-EXT? and if true exit then
   a u s" tools/ptx/" LINT-STARTS-WITH?
   a u s" -cg.f" HAS-EXT? and ;

: WATCH$ ( n -- ptr u8 n ) {: idx:n :}
   WATCH-PATHS idx FS-PATH-CAP * +
   WATCH-US idx cells + @ ;

: KNOWN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup WATCH# @ < while
      dup WATCH$ a u LINT-STR= if drop true exit then
      1+
   repeat drop false ;

: WATCH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u KNOWN? if exit then
   WATCH# @ WATCH-MAX < 0= if E-PERF-CAP throw then
   u FS-PATH-CAP > if E-PERF-CAP throw then
   a WATCH-PATHS WATCH# @ FS-PATH-CAP * + u LINT-BMOVE
   u WATCH-US WATCH# @ cells + !
   WATCH# @ 1+ WATCH# ! ;

: BAD-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-BAD-ROW " OUT REG-PATH$ OUT
   s" : invalid added registry row: " OUT a u OUT
   LF-C EMIT-C ;

: MISSING ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-ROW-MISSING " OUT a u OUT
   s" : kernel codegen changed without a profile/waiver row in " OUT
   REG-PATH$ OUT LF-C EMIT-C ;

: REG-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PERF:LINE-DATA? 0= if exit then
   a u PERF:LINE-OK? if
      ROWS+ @ 1+ ROWS+ !
   else
      a u BAD-ROW
   then ;

: ADDED ( ptr u8 n -- )
   CUR-REG @ 0<> if REG-LINE else 2drop then ;

: MARK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WATCHED? if a u WATCH+ then ;

: NEW! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MARK
   a u REG-PATH$ LINT-STR= CUR-REG ! ;

: SECTION ( ptr u8 n ptr u8 n DIFF:form bool -- )
   {: oa:ptr ou:n na:ptr nu:n kind:DIFF:form body:bool :}
   oa ou MARK na nu NEW!
   kind drop body drop ;

: EVENT ( DIFF:event -- )
   MATCH DIFF:event
      none OF ENDOF
      section OF
         DIFF:SECTION-OLD$ DIFF:SECTION-NEW$
         DIFF:SECTION-FORM DIFF:SECTION-BODY? SECTION
      ENDOF
      hunk OF ENDOF
      add OF DIFF:CONTENT$ ADDED ENDOF
      context OF ENDOF
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
   DIFF:RESET
   false CUR-REG ! ;

: MISSING-CHECK ( -- )
   ROWS+ @ 0 > if exit then
   0 begin dup WATCH# @ < while
      dup WATCH$ MISSING 1+
   repeat drop ;

public

: RESET ( -- )
   DIFF:RESET
   0 BAD ! 0 ROWS+ ! 0 WATCH# ! false CUR-REG ! ;

: SOURCE ( ptr u8 n -- )
   SOURCE-LINES ;

: FILE ( ptr u8 n -- )
   LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT SOURCE
   END-ARTIFACT ;

: FINISH ( -- )
   DIFF:FINISH EVENT
   MISSING-CHECK
   BAD @ 0 > if 1 throw then ;

;package

\ diff-frame-write.f - checked framed unified-diff artifact encoder.

require src/core/sha256.f
require lib/prelude.f
require lib/string.f
require tools/lint/diff.f

package DIFF-WRITE
private

$20 constant DIGEST-U
$53 constant SECTION-C
$54 constant TRAILER-C
1 constant FRAME-VERSION
7 constant RESERVED-U
32 constant SECTION-FIXED-U
41 constant TRAILER-U
32 constant HEADER-FIXED-U

PTR-VARIABLE OUT-A
variable OUT-CAP
variable OUT-U
variable SECTION-N

create DIGEST DIGEST-U allot

: OUT-A@ ( -- ptr u8 )
   OUT-A @ ;

: OUT-A! ( ptr u8 -- )
   OUT-A ! ;

: NEED ( n -- ) {: add:n :}
   add 0 < if E-DIFF-FRAME-CAP throw then
   OUT-U @ add + OUT-U @ < if E-DIFF-FRAME-CAP throw then
   OUT-U @ add + OUT-CAP @ > if E-DIFF-FRAME-CAP throw then ;

: U8 ( n -- ) {: value:n :}
   value 0 < value $FF > or if E-DIFF-SYNTAX throw then
   1 NEED
   value OUT-A@ OUT-U @ + c!
   OUT-U @ 1+ OUT-U ! ;

: U64 ( n -- ) {: value:n :}
   value 0 < if E-DIFF-SYNTAX throw then
   0 begin dup 8 < while
      value over 8 * rshift $FF and U8
      1+
   repeat drop ;

: BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u NEED
   a OUT-A@ OUT-U @ + u BYTE-COPY
   OUT-U @ u + OUT-U ! ;

: SIZE+ ( n n -- n ) {: total:n add:n :}
   total 0 < add 0 < or if E-DIFF-FRAME-CAP throw then
   total add + total < if E-DIFF-FRAME-CAP throw then
   total add + ;

: STATUS-BYTE ( DIFF:status -- n )
   MATCH DIFF:status
      modified OF 0 ENDOF
      added    OF 1 ENDOF
      removed  OF 2 ENDOF
      renamed  OF 3 ENDOF
      copied   OF 4 ENDOF
   ;MATCH ;

: FORM-BYTE ( DIFF:form -- n )
   MATCH DIFF:form
      text   OF 0 ENDOF
      binary OF 1 ENDOF
      mode   OF 2 ENDOF
      empty  OF 3 ENDOF
      pure   OF 4 ENDOF
      gitlink OF 5 ENDOF
   ;MATCH ;

: BOOL-BYTE ( bool -- n )
   if 1 else 0 then ;

: PATH ( bool ptr u8 n -- ) {: present:bool a:ptr u:n :}
   present if
      u 0 <= if E-DIFF-SYNTAX throw then
   else
      u 0<> if E-DIFF-SYNTAX throw then
   then
   u U64
   a u BYTES ;

: HEADER ( ptr u8 n ptr u8 n -- ) {: from:ptr fromu:n to:ptr tou:n :}
   fromu 0 <= tou 0 <= or if E-DIFF-SYNTAX throw then
   from fromu DIFF:COMMIT-ID? 0= if E-DIFF-SYNTAX throw then
   to tou DIFF:COMMIT-ID? 0= if E-DIFF-SYNTAX throw then
   s" HABUDIF2" BYTES
   FRAME-VERSION U8
   0 begin dup RESERVED-U < while
      0 U8
      1+
   repeat drop
   fromu U64 from fromu BYTES
   tou U64 to tou BYTES ;

: CHECK-SECTION ( DIFF:status DIFF:form bool bool bool ptr u8 n bool ptr u8 n ptr u8 n -- )
   {: change:DIFF:status expected:DIFF:form body:bool mode:bool old?:bool oa:ptr ou:n new?:bool na:ptr nu:n raw:ptr rawu:n :}
   change old? oa ou new? na nu raw rawu DIFF:VALIDATE-SECTION
   {: got:DIFF:form gotbody:bool gotmode:bool :}
   got FORM-BYTE expected FORM-BYTE <> if E-DIFF-SYNTAX throw then
   gotbody BOOL-BYTE body BOOL-BYTE <> if E-DIFF-SYNTAX throw then
   gotmode BOOL-BYTE mode BOOL-BYTE <> if E-DIFF-SYNTAX throw then ;

public

: HEADER-SIZE ( n n -- n ) {: fromu:n tou:n :}
   HEADER-FIXED-U fromu SIZE+ tou SIZE+ ;

: SECTION-SIZE ( n n n n -- n ) {: total:n oldu:n newu:n rawu:n :}
   total SECTION-FIXED-U SIZE+ oldu SIZE+ newu SIZE+ rawu SIZE+ ;

: FINISH-SIZE ( n -- n )
   TRAILER-U SIZE+ ;

: START ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: out:ptr cap:n from:ptr fromu:n to:ptr tou:n :}
   out OUT-A! cap OUT-CAP !
   0 OUT-U !
   0 SECTION-N !
   from fromu to tou HEADER ;

: SECTION ( DIFF:status DIFF:form bool bool bool ptr u8 n bool ptr u8 n ptr u8 n -- )
   {: change:DIFF:status shape:DIFF:form body:bool mode:bool old?:bool oa:ptr ou:n new?:bool na:ptr nu:n raw:ptr rawu:n :}
   change shape body mode old? oa ou new? na nu raw rawu CHECK-SECTION
   SECTION-C U8
   change STATUS-BYTE U8
   shape FORM-BYTE U8
   body BOOL-BYTE U8
   mode BOOL-BYTE U8
   old? BOOL-BYTE U8
   new? BOOL-BYTE U8
   0 U8
   old? oa ou PATH
   new? na nu PATH
   rawu U64 raw rawu BYTES
   SECTION-N @ $7FFFFFFFFFFFFFFF = if E-DIFF-FRAME-CAP throw then
   SECTION-N @ 1+ SECTION-N ! ;

: FINISH ( -- ptr u8 n )
   TRAILER-C U8
   SECTION-N @ U64
   OUT-A@ OUT-U @ DIGEST SHA256
   DIGEST DIGEST-U BYTES
   OUT-A@ OUT-U @ ;

;package

\ diff-frame.f - validated framed unified-diff artifact reader.

require src/core/sha256.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require tools/lint/diff.f

package DIFF
private

$20 constant DIGEST-U
$48 constant MAGIC-H
$41 constant MAGIC-A
$42 constant MAGIC-B
$55 constant MAGIC-U-C
$44 constant MAGIC-D
$49 constant MAGIC-I
$46 constant MAGIC-F
$32 constant MAGIC-2
$53 constant SECTION-C
$54 constant TRAILER-C
1 constant FRAME-VERSION
8 constant MAGIC-LEN
7 constant RESERVED-U
75 constant MIN-ART-U

PTR-VARIABLE ART-A
variable ART-U
variable ART-CAP
PTR-VARIABLE VIEW-A
variable VIEW-CAP
PTR-VARIABLE FILE-VIEW-A
variable FILE-VIEW-CAP
variable CUR
variable LIMIT
variable DATA-START
variable FRAME-N
variable ITER-N
variable SEC-IDX
variable OPENED
variable ITER-IN
variable U64-I
variable U64-V
PTR-VARIABLE FROM-ID-A
variable FROM-ID-U
PTR-VARIABLE TO-ID-A
variable TO-ID-U

1 LAYOUT-BUFFER SEC-STATUS status
1 LAYOUT-BUFFER SEC-FORM form
variable SEC-BODY
variable SEC-MODE
variable SEC-OLD-PRESENT
variable SEC-NEW-PRESENT
PTR-VARIABLE SEC-OLD-A
variable SEC-OLD-U
PTR-VARIABLE SEC-NEW-A
variable SEC-NEW-U
PTR-VARIABLE SEC-RAW-A
variable SEC-RAW-U

create GOT-DIGEST DIGEST-U allot

: ART-A@ ( -- ptr u8 )
   ART-A @ ;

: SEC-OLD-A@ ( -- ptr u8 )
   SEC-OLD-A @ ;

: SEC-NEW-A@ ( -- ptr u8 )
   SEC-NEW-A @ ;

: SEC-RAW-A@ ( -- ptr u8 )
   SEC-RAW-A @ ;

: ART-A! ( ptr u8 -- )
   ART-A ! ;

: VIEW-A! ( ptr u8 -- )
   VIEW-A ! ;

: SEC-OLD-A! ( ptr u8 -- )
   SEC-OLD-A ! ;

: SEC-NEW-A! ( ptr u8 -- )
   SEC-NEW-A ! ;

: SEC-RAW-A! ( ptr u8 -- )
   SEC-RAW-A ! ;

: FROM-ID$ ( -- ptr u8 n )
   FROM-ID-A @ FROM-ID-U @ ;

: TO-ID$ ( -- ptr u8 n )
   TO-ID-A @ TO-ID-U @ ;

: SEC-STATUS-AT ( -- ptr status )
   0 SEC-STATUS ;

: SEC-FORM-AT ( -- ptr form )
   0 SEC-FORM ;

: SEC-STATUS@ ( -- status )
   SEC-STATUS-AT @ ;

: SEC-FORM@ ( -- form )
   SEC-FORM-AT @ ;

: SEC-STATUS! ( status -- )
   SEC-STATUS-AT ! ;

: SEC-FORM! ( form -- )
   SEC-FORM-AT ! ;

: SEC-OLD? ( -- bool )
   SEC-OLD-PRESENT @ if true else false then ;

: SEC-NEW? ( -- bool )
   SEC-NEW-PRESENT @ if true else false then ;

: SEC-BODY? ( -- bool )
   SEC-BODY @ if true else false then ;

: SEC-MODE? ( -- bool )
   SEC-MODE @ if true else false then ;

: ROOM? ( n -- bool ) {: need:n :}
   need 0 >= CUR @ need + CUR @ >= and
   CUR @ need + LIMIT @ <= and ;

: NEED ( n -- )
   ROOM? 0= if E-DIFF-SYNTAX throw then ;

: U8 ( -- n )
   1 NEED
   ART-A@ CUR @ + c@
   CUR @ 1+ CUR ! ;

: ART-C@ ( n -- n ) {: off:n :}
   ART-A@ off + c@ ;

: U64 ( -- n )
   8 NEED
   CUR @ 7 + ART-C@ $80 >= if E-DIFF-SYNTAX throw then
   0 U64-I !
   0 U64-V !
   begin U64-I @ 8 < while
      CUR @ U64-I @ + ART-C@
      U64-I @ 8 * lshift U64-V @ or U64-V !
      U64-I @ 1+ U64-I !
   repeat
   CUR @ 8 + CUR !
   U64-V @ ;

: SPAN ( n -- ptr u8 n ) {: u:n :}
   u NEED
   ART-A@ CUR @ + u
   CUR @ u + CUR ! ;

: BOOL-TAG ( -- bool )
   U8 dup 0= if drop false exit then
   1 = if true exit then
   E-DIFF-SYNTAX throw ;

: BYTE>STATUS ( n -- status )
   case
      0 of DIFF-STATUS:MODIFIED endof
      1 of DIFF-STATUS:ADDED endof
      2 of DIFF-STATUS:REMOVED endof
      3 of DIFF-STATUS:RENAMED endof
      4 of DIFF-STATUS:COPIED endof
      E-DIFF-SYNTAX throw
   endcase ;

: STATUS>BYTE ( status -- n )
   MATCH status
      modified OF 0 ENDOF
      added    OF 1 ENDOF
      removed  OF 2 ENDOF
      renamed  OF 3 ENDOF
      copied   OF 4 ENDOF
   ;MATCH ;

: BYTE>FORM ( n -- form )
   case
      0 of DIFF-FORM:TEXT endof
      1 of DIFF-FORM:BINARY endof
      2 of DIFF-FORM:MODE endof
      3 of DIFF-FORM:EMPTY endof
      4 of DIFF-FORM:PURE endof
      5 of DIFF-FORM:GITLINK endof
      E-DIFF-SYNTAX throw
   endcase ;

: FORM>BYTE ( form -- n )
   MATCH form
      text   OF 0 ENDOF
      binary OF 1 ENDOF
      mode   OF 2 ENDOF
      empty  OF 3 ENDOF
      pure   OF 4 ENDOF
      gitlink OF 5 ENDOF
   ;MATCH ;

: BOOL>N ( bool -- n )
   if 1 else 0 then ;

: SEC-OLD$ ( -- ptr u8 n )
   SEC-OLD-A@ SEC-OLD-U @ ;

: SEC-NEW$ ( -- ptr u8 n )
   SEC-NEW-A@ SEC-NEW-U @ ;

: SEC-RAW$ ( -- ptr u8 n )
   SEC-RAW-A@ SEC-RAW-U @ ;

: NEXT-CAP ( n n -- n ) {: need:n current:n :}
   current 1 < if 1 else current then
   begin dup need < while
      dup $3FFFFFFFFFFFFFFF > if drop need exit then
      2 *
   repeat ;

: OWN-ARTIFACT ( ptr u8 n -- ) {: a:ptr u:n :}
   u ART-CAP @ > if
      u ART-CAP @ NEXT-CAP {: cap:n :}
      cap MEM-ALLOC-BYTES drop ART-A!
      cap ART-CAP !
   then
   a ART-A@ u BYTE-COPY
   u ART-U ! ;

: DETACH ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u VIEW-CAP @ > if
      u VIEW-CAP @ NEXT-CAP {: cap:n :}
      cap MEM-ALLOC-BYTES drop VIEW-A!
      cap VIEW-CAP !
   then
   u 0 > if a VIEW-A @ u BYTE-COPY then
   VIEW-A @ u ;

: DETACH-FILE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u FILE-VIEW-CAP @ > if
      u FILE-VIEW-CAP @ NEXT-CAP {: cap:n :}
      cap MEM-ALLOC-BYTES drop FILE-VIEW-A !
      cap FILE-VIEW-CAP !
   then
   u 0 > if a FILE-VIEW-A @ u BYTE-COPY then
   FILE-VIEW-A @ u ;

: READ-SECTION ( -- )
   U8 SECTION-C <> if E-DIFF-SYNTAX throw then
   U8 BYTE>STATUS SEC-STATUS!
   U8 BYTE>FORM SEC-FORM!
   BOOL-TAG SEC-BODY !
   BOOL-TAG SEC-MODE !
   BOOL-TAG SEC-OLD-PRESENT !
   BOOL-TAG SEC-NEW-PRESENT !
   U8 0<> if E-DIFF-SYNTAX throw then
   U64 SPAN SEC-OLD-U ! SEC-OLD-A!
   U64 SPAN SEC-NEW-U ! SEC-NEW-A!
   U64 SPAN SEC-RAW-U ! SEC-RAW-A! ;

: CHECK-SECTION ( -- )
   SEC-STATUS@ SEC-OLD? SEC-OLD$ SEC-NEW? SEC-NEW$ SEC-RAW$
   VALIDATE-SECTION {: got:form body:bool mode:bool :}
   got FORM>BYTE SEC-FORM@ FORM>BYTE <> if E-DIFF-SYNTAX throw then
   body BOOL>N SEC-BODY? BOOL>N <> if E-DIFF-SYNTAX throw then
   mode BOOL>N SEC-MODE? BOOL>N <> if E-DIFF-SYNTAX throw then ;

: MAGIC? ( -- bool )
   8 NEED
   ART-A@ CUR @ + dup c@ MAGIC-H =
   over 1+ c@ MAGIC-A = and
   over 2 + c@ MAGIC-B = and
   over 3 + c@ MAGIC-U-C = and
   over 4 + c@ MAGIC-D = and
   over 5 + c@ MAGIC-I = and
   over 6 + c@ MAGIC-F = and
   swap 7 + c@ MAGIC-2 = and ;

: HEADER ( -- )
   MAGIC? 0= if E-DIFF-SYNTAX throw then
   CUR @ MAGIC-LEN + CUR !
   U8 FRAME-VERSION <> if E-DIFF-SYNTAX throw then
   0 begin dup RESERVED-U < while
      U8 0<> if E-DIFF-SYNTAX throw then
      1+
   repeat drop
   U64 dup 0= if E-DIFF-SYNTAX throw then SPAN
   2dup COMMIT-ID? 0= if 2drop E-DIFF-SYNTAX throw then
   FROM-ID-U ! FROM-ID-A !
   U64 dup 0= if E-DIFF-SYNTAX throw then SPAN
   2dup COMMIT-ID? 0= if 2drop E-DIFF-SYNTAX throw then
   TO-ID-U ! TO-ID-A !
   CUR @ DATA-START ! ;

: DIGEST-VALID? ( -- bool )
   ART-U @ DIGEST-U < if false exit then
   ART-A@ ART-U @ DIGEST-U - GOT-DIGEST SHA256
   GOT-DIGEST DIGEST-U ART-A@ ART-U @ DIGEST-U - + DIGEST-U STR= ;

: TRAILER ( -- )
   U8 TRAILER-C <> if E-DIFF-SYNTAX throw then
   U64 FRAME-N @ <> if E-DIFF-SYNTAX throw then
   CUR @ LIMIT @ <> if E-DIFF-SYNTAX throw then ;

: VALIDATE ( -- )
   HEADER
   0 FRAME-N !
   begin
      CUR @ LIMIT @ >= if E-DIFF-SYNTAX throw then
      ART-A@ CUR @ + c@ TRAILER-C = if TRAILER exit then
      READ-SECTION
      CHECK-SECTION
      FRAME-N @ $7FFFFFFFFFFFFFFF = if E-DIFF-SYNTAX throw then
      FRAME-N @ 1+ FRAME-N !
   again ;

: START-SECTION ( -- )
   READ-SECTION
   ITER-N @ SEC-IDX !
   ITER-N @ 1+ ITER-N !
   SEC-STATUS@ SEC-OLD? SEC-OLD$ SEC-NEW? SEC-NEW$ SEC-RAW$ RAW-BEGIN
   true ITER-IN ! ;

: EVENT-STEP ( -- ptr u8 n n event bool )
   RAW-STEP {: a:ptr u:n meta:n kind:event :}
   kind MATCH event
      none    OF a u meta DIFF-EVENT:NONE false ENDOF
      file    OF a u DETACH-FILE meta DIFF-EVENT:FILE true ENDOF
      hunk    OF a u DETACH meta DIFF-EVENT:HUNK true ENDOF
      add     OF a u DETACH meta DIFF-EVENT:ADD true ENDOF
      context OF a u DETACH meta DIFF-EVENT:CONTEXT true ENDOF
      delete  OF a u DETACH meta DIFF-EVENT:DELETE true ENDOF
   ;MATCH ;

: END-SECTION ( -- )
   RAW-END
   RAW-FORM {: got:form body:bool mode:bool :}
   got FORM>BYTE SEC-FORM@ FORM>BYTE <> if E-DIFF-SYNTAX throw then
   body BOOL>N SEC-BODY? BOOL>N <> if E-DIFF-SYNTAX throw then
   mode BOOL>N SEC-MODE? BOOL>N <> if E-DIFF-SYNTAX throw then
   false ITER-IN ! ;

: NEED-OPEN ( -- )
   OPENED @ 0= if E-DIFF-SYNTAX throw then ;

: NEED-SECTION ( -- )
   NEED-OPEN
   ITER-IN @ 0= if E-DIFF-SYNTAX throw then ;

public

: FROM$ ( -- ptr u8 n )
   NEED-OPEN
   FROM-ID$ DETACH ;

: TO$ ( -- ptr u8 n )
   NEED-OPEN
   TO-ID$ DETACH ;

: SECTION-STATUS ( -- status )
   NEED-SECTION
   SEC-STATUS@ ;

: SECTION-FORM ( -- form )
   NEED-SECTION
   SEC-FORM@ ;

: SECTION-BODY? ( -- bool )
   NEED-SECTION
   SEC-BODY? ;

: SECTION-MODE? ( -- bool )
   NEED-SECTION
   SEC-MODE? ;

: SECTION-OLD? ( -- bool )
   NEED-SECTION
   SEC-OLD? ;

: SECTION-OLD$ ( -- ptr u8 n )
   NEED-SECTION
   SEC-OLD$ DETACH ;

: SECTION-NEW? ( -- bool )
   NEED-SECTION
   SEC-NEW? ;

: SECTION-NEW$ ( -- ptr u8 n )
   NEED-SECTION
   SEC-NEW$ DETACH ;

: SECTION-COUNT ( -- n )
   NEED-OPEN
   FRAME-N @ ;

: SECTION-INDEX ( -- n )
   NEED-SECTION
   SEC-IDX @ ;

: OPEN ( ptr u8 n -- ) {: a:ptr u:n :}
   u MIN-ART-U < if E-DIFF-SYNTAX throw then
   a u OWN-ARTIFACT
   u DIGEST-U - LIMIT !
   0 CUR !
   false OPENED !
   false ITER-IN !
   0 ITER-N !
   0 SEC-IDX !
   DIGEST-VALID? 0= if E-DIFF-SYNTAX throw then
   VALIDATE
   DATA-START @ CUR !
   true OPENED ! ;

: NEXT? ( -- ptr u8 n n event bool )
   OPENED @ 0= if E-DIFF-SYNTAX throw then
   begin
      ITER-IN @ if
         RAW-I @ RAW-U @ < if
            EVENT-STEP if true exit then
            drop drop 2drop
         else
            END-SECTION
         then
      else
         CUR @ LIMIT @ >= if E-DIFF-SYNTAX throw then
         ART-A@ CUR @ + c@ TRAILER-C = if
            TRAILER
            false OPENED !
            NONE-EVENT false exit
         then
         START-SECTION
      then
   again ;

;package

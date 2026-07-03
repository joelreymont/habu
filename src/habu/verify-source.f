\ verify-source.f - pre-compile checked source verifier.
\
\ Load after checker/render/hook support. This scanner verifies colon
\ definitions with CHECK! and records top-level defining words that the checker
\ needs before those definitions are compiled by the native compiler.

package VERIFY

variable SOURCE-A
variable SOURCE-U
variable SCAN-I
variable SKIP-STRINGS
variable FOUND
variable TOKEN-START
variable TOKEN-A
variable TOKEN-U
variable STRING-DONE
variable BODY-U
variable LINE-N
variable COL-N
variable POS
variable BASE-LINE
variable BASE-COL
variable BASE-BYTE
variable BACKSLASHES
variable ESCAPED-STRING

create BODY-BUF BODYBUF-CAP allot

: SOURCE@ ( -- ptr u8 )
   SOURCE-A 0 ptr-field @ ;

: BASE-RESET ( -- )
   1 BASE-LINE !
   1 BASE-COL !
   0 BASE-BYTE ! ;

: SOURCE! ( ptr u8 n -- )
   BASE-RESET
   SOURCE-U !
   SOURCE-A ! ;

: SOURCE-AT! ( ptr u8 n n n n -- ) {: a:ptr u:n line:n col:n byte:n :}
   a u SOURCE!
   line BASE-LINE !
   col BASE-COL !
   byte BASE-BYTE ! ;

: SKIP-WS ( -- )
   BEGIN SCAN-I @ SOURCE-U @ < IF SOURCE@ SCAN-I @ + c@ 33 < ELSE 0 0= 0= THEN WHILE
      SCAN-I @ 1 + SCAN-I !
   REPEAT ;

: SKIP-PAST ( n -- ) {: ch:n :}
   0 FOUND !
   BEGIN SCAN-I @ SOURCE-U @ < WHILE
      SOURCE@ SCAN-I @ + c@  SCAN-I @ 1 + SCAN-I !  ch = IF -1 FOUND ! EXIT THEN
   REPEAT ;

: NEXT-RAW ( -- ptr u8 n )
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= IF SOURCE@ 0 EXIT THEN
   SCAN-I @ TOKEN-START !
   BEGIN SCAN-I @ SOURCE-U @ < IF SOURCE@ SCAN-I @ + c@ 32 > ELSE 0 0= 0= THEN WHILE
      SCAN-I @ 1 + SCAN-I !
   REPEAT
   SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ - ;

: SC-LEAD? ( n -- bool )
   dup $73 = over $53 = or over $63 = or swap $43 = or ;

: STRING-LEAD? ( n -- bool )
   dup SC-LEAD? swap $2E = or ;

: NORMAL-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: ESCAPED-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 3 <> IF 0 0= 0= EXIT THEN
   a 1 BYTE@ $5C <> IF 0 0= 0= EXIT THEN
   a 2 BYTE@ $22 <> IF 0 0= 0= EXIT THEN
   a 0 BYTE@ STRING-LEAD? ;

: STRING-OPENER? ( ptr u8 n -- bool )
   2dup NORMAL-STRING-OPENER? IF 2drop 0 0= EXIT THEN
   ESCAPED-STRING-OPENER? ;

: SKIP-ESCAPED-QUOTE ( -- )
   0 FOUND !
   BEGIN SCAN-I @ SOURCE-U @ < WHILE
      SOURCE@ SCAN-I @ + c@  SCAN-I @ 1 + SCAN-I !
      dup 92 = IF
         drop
         SCAN-I @ SOURCE-U @ < IF SCAN-I @ 1 + SCAN-I ! THEN
      ELSE
         34 = IF -1 FOUND ! EXIT THEN
      THEN
   REPEAT ;

: NEXT ( -- ptr u8 n )
   BEGIN
      NEXT-RAW
      dup 0= IF EXIT THEN
      2dup 1 = swap c@ 92 = and IF 2drop 10 SKIP-PAST ELSE
      2dup 1 = swap c@ 40 = and IF 2drop 41 SKIP-PAST ELSE
      SKIP-STRINGS @ 0= 0= IF
         2dup ESCAPED-STRING-OPENER? IF 2drop SKIP-ESCAPED-QUOTE ELSE
         2dup NORMAL-STRING-OPENER? IF 2drop 34 SKIP-PAST ELSE EXIT THEN THEN
      ELSE EXIT THEN
      THEN THEN
   AGAIN ;

: NEXT-SCAN ( -- ptr u8 n )
   -1 SKIP-STRINGS !
   NEXT ;

: NEXT-BODY ( -- ptr u8 n )
   0 SKIP-STRINGS !
   NEXT ;

: RAW! ( -- )
   NEXT-RAW  TOKEN-U !  TOKEN-A ! ;

: BODY! ( -- )
   NEXT-BODY  TOKEN-U !  TOKEN-A ! ;

: BODY-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   BODY-U @ u + 1 + BODYBUF-CAP > IF s" verify-source: check body too long" 74 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  BODY-BUF BODY-U @ + c!
      BODY-U @ 1 + BODY-U !
      1 +
   REPEAT drop
   32 BODY-BUF BODY-U @ + c!  BODY-U @ 1 + BODY-U ! ;

: LINE-COL ( n -- n n ) {: idx:n :}
   1 LINE-N !
   1 COL-N !
   0 POS !
   BEGIN POS @ idx <  POS @ SOURCE-U @ < and WHILE
      SOURCE@ POS @ + c@ 10 = IF
         LINE-N @ 1 + LINE-N !
         1 COL-N !
      ELSE
         COL-N @ 1 + COL-N !
      THEN
      POS @ 1 + POS !
   REPEAT
   LINE-N @ COL-N @ ;

: ABS-ORIGIN ( n -- n n n ) {: idx:n :}
   idx LINE-COL {: line:n col:n :}
   BASE-LINE @ line + 1 -
   line 1 = IF BASE-COL @ col + 1 - ELSE col THEN
   BASE-BYTE @ idx + ;

: ORIGIN! ( n -- )
   ABS-ORIGIN DIAG-ORIGIN! ;

: MAYBE-SIGNATURE ( -- )
   SKIP-WS
   SCAN-I @ SOURCE-U @ < IF
      SOURCE@ SCAN-I @ + c@ 40 = IF
         SCAN-I @ TOKEN-START !
         41 SKIP-PAST
         FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
         SOURCE@ TOKEN-START @ +  SCAN-I @ TOKEN-START @ -  BODY-APPEND
      THEN
   THEN ;

: REQUIRE-SIGNATURE ( -- ptr u8 n )
   SKIP-WS
   SCAN-I @ SOURCE-U @ >= IF s" verify-source: missing signature" 74 die THEN
   SOURCE@ SCAN-I @ + c@ 40 <> IF s" verify-source: missing signature" 74 die THEN
   SCAN-I @ 1+ TOKEN-START !
   41 SKIP-PAST
   FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
   SOURCE@ TOKEN-START @ + SCAN-I @ TOKEN-START @ - 1 - ;

: ENDS-QUOTE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > IF a u + 1 - c@ 34 = ELSE 0 0= 0= THEN ;

: ENDS-ESCAPED-QUOTE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF 0 0= 0= EXIT THEN
   a u + 1 - c@ 34 <> IF 0 0= 0= EXIT THEN
   0 BACKSLASHES !
   u 1 - POS !
   BEGIN POS @ 0 > WHILE
      a POS @ 1 - + c@ 92 = IF
         BACKSLASHES @ 1 + BACKSLASHES !
         POS @ 1 - POS !
      ELSE
         BACKSLASHES @ 1 and 0= EXIT
      THEN
   REPEAT
   BACKSLASHES @ 1 and 0= ;

: APPEND-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ESCAPED-STRING-OPENER? ESCAPED-STRING !
   a u BODY-APPEND
   BEGIN
      RAW!
      TOKEN-U @ 0= IF s" verify-source: unterminated string" 74 die THEN
      ESCAPED-STRING @ IF
         TOKEN-A @ TOKEN-U @ ENDS-ESCAPED-QUOTE?
      ELSE
         TOKEN-A @ TOKEN-U @ ENDS-QUOTE?
      THEN STRING-DONE !
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      STRING-DONE @ IF EXIT THEN
   AGAIN ;

: SKIP-STRING-REST ( ptr u8 n -- ) {: a:ptr u:n :}
   a u ESCAPED-STRING-OPENER? ESCAPED-STRING !
   ESCAPED-STRING @ IF a u ENDS-ESCAPED-QUOTE? ELSE a u ENDS-QUOTE? THEN IF exit THEN
   BEGIN
      RAW!
      TOKEN-U @ 0= IF s" verify-source: unterminated string" 74 die THEN
      ESCAPED-STRING @ IF
         TOKEN-A @ TOKEN-U @ ENDS-ESCAPED-QUOTE?
      ELSE
         TOKEN-A @ TOKEN-U @ ENDS-QUOTE?
      THEN IF EXIT THEN
   AGAIN ;

: PARSE-NEXT? ( ptr u8 n -- bool )
   2dup s" char" CORE-STR= IF 2drop 0 0= exit THEN
   s" [char]" CORE-STR= ;

: APPEND-NEXT-BODY ( -- )
   BODY!
   TOKEN-U @ 0= IF s" verify-source: missing parsed token" 74 die THEN
   TOKEN-A @ TOKEN-U @ BODY-APPEND ;

: APPEND-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ PARSE-NEXT? IF
      TOKEN-A @ TOKEN-U @ BODY-APPEND
      APPEND-NEXT-BODY
      exit
   THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF
      TOKEN-A @ TOKEN-U @ APPEND-STRING
   ELSE
      TOKEN-A @ TOKEN-U @ BODY-APPEND
   THEN ;

: SKIP-NEXT-BODY ( -- )
   BODY!
   TOKEN-U @ 0= IF s" verify-source: missing parsed token" 74 die THEN ;

: SKIP-BODY-TOKEN ( -- )
   TOKEN-A @ TOKEN-U @ PARSE-NEXT? IF SKIP-NEXT-BODY exit THEN
   TOKEN-A @ TOKEN-U @ STRING-OPENER? IF TOKEN-A @ TOKEN-U @ SKIP-STRING-REST THEN ;

TRUSTED: CHECK-BODY ( ptr u8 n -- n )
   CHECK! dup 1 = JSON-DIAGS @ 0= and DIAGXT @ 0 <> and IF DIAGXT @ execute THEN ;

: VERIFY-BODY ( -- )
   BODY-BUF BODY-U @ CHECK-BODY  dup -1 <> IF 70 throw THEN drop ;

TRUSTED: CHECK-DOES-BODY ( ptr u8 n ptr u8 n -- n )
   CHECK-DOES! ;

: VERIFY-DOES-BODY ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   BODY-BUF BODY-U @ sig sigu CHECK-DOES-BODY
   dup -1 <> IF 70 throw THEN drop ;

: VERIFY-DOES ( -- )
   VERIFY-BODY
   REQUIRE-SIGNATURE {: sig:ptr sigu:n :}
   0 BODY-U !
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated does body" 74 die THEN
      BODY-U @ 0= IF TOKEN-START @ ORIGIN! THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF sig sigu VERIFY-DOES-BODY EXIT THEN
      APPEND-BODY-TOKEN
   AGAIN ;

TRUSTED: TRUST-SIGNATURE ( ptr u8 n ptr u8 n -- )
   TRUST ;

: FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> IF 0 0= 0= EXIT THEN
   0 BEGIN dup u < WHILE
      dup a + c@ FOLD-C
      over b + c@ FOLD-C <> IF drop 0 0= 0= EXIT THEN
      1+
   REPEAT drop 0 0= ;

: TRUST-NEXT ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   NEXT-SCAN
   dup 0= IF s" verify-source: missing defining-word name" 74 die THEN
   sig sigu TRUST-SIGNATURE ;

: TRUST-DEFER-SIGNATURE ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu REQUIRE-SIGNATURE TRUST-SIGNATURE
   name nameu CHECKER-DEFER ;

: TRUST-DEFER ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing defer name" 74 die THEN
   name nameu TRUST-DEFER-SIGNATURE ;

: SKIP-TRUSTED-BODY ( -- )
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated trusted definition" 74 die THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF EXIT THEN
      SKIP-BODY-TOKEN
   AGAIN ;

: TRUSTED-DEFINITION ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing trusted name" 74 die THEN
   name nameu REQUIRE-SIGNATURE TRUST-SIGNATURE
   SKIP-TRUSTED-BODY ;

: UNDEFINE-WORD ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing undefine name" 74 die THEN
   name nameu CHECKER-UNDEFINE ;

: RECORD-PACKAGE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing package name" 74 die THEN
   name nameu CHECKER-PACKAGE ;

: RECORD-PUBLIC ( -- )
   CHECKER-PUBLIC ;

: RECORD-PRIVATE ( -- )
   CHECKER-PRIVATE ;

: RECORD-END-PACKAGE ( -- )
   CHECKER-END-PACKAGE ;

: RECORD-DEFTYPE ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing deftype name" 74 die THEN
   name nameu CHECKER-DEFTYPE ;

: RECORD-DEFLINEAR ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing deflinear name" 74 die THEN
   name nameu CHECKER-DEFLINEAR ;

: VALUE-RECORD-END? ( ptr u8 n -- bool )
   s" END-VALUE-RECORD" STR=CI ;

: RECORD-VALUE-RECORD ( -- )
   NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing value-record name" 74 die THEN
   0 BODY-U !
   BEGIN
      NEXT-SCAN
      dup 0= IF s" verify-source: missing END-VALUE-RECORD" 74 die THEN
      2dup VALUE-RECORD-END? IF
         2drop
         name nameu BODY-BUF BODY-U @ CHECKER-DEFRECORD
         EXIT
      THEN
      BODY-APPEND
   AGAIN ;

: RECORD-DEFINER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" STR=CI IF RECORD-PACKAGE 0 0= EXIT THEN
   a u s" public" STR=CI IF RECORD-PUBLIC 0 0= EXIT THEN
   a u s" private" STR=CI IF RECORD-PRIVATE 0 0= EXIT THEN
   a u s" end-package" STR=CI IF RECORD-END-PACKAGE 0 0= EXIT THEN
   a u s" deftype" STR=CI IF RECORD-DEFTYPE 0 0= EXIT THEN
   a u s" deflinear" STR=CI IF RECORD-DEFLINEAR 0 0= EXIT THEN
   a u s" value-record" STR=CI IF RECORD-VALUE-RECORD 0 0= EXIT THEN
   a u s" constant" STR=CI IF s" -- a" TRUST-NEXT 0 0= EXIT THEN
   a u s" create" STR=CI IF s" -- ptr a" TRUST-NEXT 0 0= EXIT THEN
   a u s" variable" STR=CI IF s" -- ptr a" TRUST-NEXT 0 0= EXIT THEN
   a u s" defer" STR=CI IF TRUST-DEFER 0 0= EXIT THEN
   a u s" trusted:" STR=CI IF TRUSTED-DEFINITION 0 0= EXIT THEN
   a u s" undefine" STR=CI IF UNDEFINE-WORD 0 0= EXIT THEN
   0 0= 0= ;

: VERIFY-DEFINITION ( -- )
   0 BODY-U !
   BODY!
   TOKEN-U @ 0= IF s" verify-source: missing word name" 74 die THEN
   TOKEN-START @ ORIGIN!
   TOKEN-A @ TOKEN-U @ BODY-APPEND
   MAYBE-SIGNATURE
   BEGIN
      BODY!
      TOKEN-U @ 0= IF s" verify-source: unterminated definition" 74 die THEN
      TOKEN-A @ TOKEN-U @ s" ;" CORE-STR= IF VERIFY-BODY EXIT THEN
      TOKEN-A @ TOKEN-U @ s" does>" CORE-STR= IF VERIFY-DOES EXIT THEN
      APPEND-BODY-TOKEN
   AGAIN ;

: VERIFY-SOURCE ( -- )
   0 SCAN-I !
   BEGIN
      NEXT-SCAN dup 0 > WHILE
      2dup s" :" CORE-STR= IF 2drop VERIFY-DEFINITION ELSE
      2dup RECORD-DEFINER? IF 2drop ELSE 2drop THEN THEN
   REPEAT 2drop ;

: THROW-RESULT ( n -- )
   dup 0= IF drop exit THEN
   throw ;

: RUN ( -- )
   [: VERIFY-SOURCE ;] catch THROW-RESULT ;

public

: SOURCE-BUF-IN-SCOPE ( ptr u8 n -- )
   SOURCE!
   RUN ;

: SOURCE-BUF-AT-IN-SCOPE ( ptr u8 n n n n -- )
   SOURCE-AT!
   RUN ;

: SOURCE-BUF ( ptr u8 n -- )
   SOURCE!
   CHECKER-CANDIDATE-SCOPE-START
   [: RUN ;] catch
   CHECKER-CANDIDATE-SCOPE-DONE
   THROW-RESULT ;

end-package

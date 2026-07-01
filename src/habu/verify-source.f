\ verify-source.f - pre-compile checked source verifier.
\
\ Load after checker/render/hook support. This scanner verifies colon
\ definitions with CHECK! and records top-level defining words that the checker
\ needs before those definitions are compiled by the native compiler.

variable VS-A
variable VS-U
variable VS-I
variable VS-T
variable VS-SKIPSTR
variable VS-FOUND
variable VS-START
variable VS-TA
variable VS-TU
variable VS-ENDQ
variable VS-L
variable VS-LINE
variable VS-COL
variable VS-POS
variable VS-BASE-LINE
variable VS-BASE-COL
variable VS-BASE-BYTE

create VS-BUF BODYBUF-CAP allot

: VS-SRC@ ( -- ptr u8 )
   VS-A 0 ptr-field @ ;

: VS-BASE-RESET ( -- )
   1 VS-BASE-LINE !
   1 VS-BASE-COL !
   0 VS-BASE-BYTE ! ;

: VS-SRC! ( ptr u8 n -- )
   VS-BASE-RESET
   VS-U !
   VS-A ! ;

: VS-SRC-AT! ( ptr u8 n n n n -- ) {: a:ptr u:n line:n col:n byte:n :}
   a u VS-SRC!
   line VS-BASE-LINE !
   col VS-BASE-COL !
   byte VS-BASE-BYTE ! ;

: VS-SKIP-WS ( -- )
   BEGIN VS-I @ VS-U @ < IF VS-SRC@ VS-I @ + c@ 33 < ELSE 0 0= 0= THEN WHILE
      VS-I @ 1 + VS-I !
   REPEAT ;

: VS-SKIP-PAST ( n -- ) {: ch:n :}
   0 VS-FOUND !
   BEGIN VS-I @ VS-U @ < WHILE
      VS-SRC@ VS-I @ + c@  VS-I @ 1 + VS-I !  ch = IF -1 VS-FOUND ! EXIT THEN
   REPEAT ;

: VS-NEXT-RAW ( -- ptr u8 n )
   VS-SKIP-WS
   VS-I @ VS-U @ >= IF VS-SRC@ 0 EXIT THEN
   VS-I @ VS-START !
   BEGIN VS-I @ VS-U @ < IF VS-SRC@ VS-I @ + c@ 32 > ELSE 0 0= 0= THEN WHILE
      VS-I @ 1 + VS-I !
   REPEAT
   VS-SRC@ VS-START @ +  VS-I @ VS-START @ - ;

: VS-OPN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 = IF
      a 1 + c@ 34 = IF
         a c@ 115 =  a c@ 46 = or  a c@ 99 = or
      ELSE 0 0= 0= THEN
   ELSE 0 0= 0= THEN ;

: VS-NEXT ( -- ptr u8 n )
   BEGIN
      VS-NEXT-RAW
      dup 0= IF EXIT THEN
      2dup 1 = swap c@ 92 = and IF 2drop 10 VS-SKIP-PAST ELSE
      2dup 1 = swap c@ 40 = and IF 2drop 41 VS-SKIP-PAST ELSE
      VS-SKIPSTR @ 0= 0= IF
         2dup VS-OPN? IF 2drop 34 VS-SKIP-PAST ELSE EXIT THEN
      ELSE EXIT THEN
      THEN THEN
   AGAIN ;

: VS-NEXT-SCAN ( -- ptr u8 n )
   -1 VS-SKIPSTR !
   VS-NEXT ;

: VS-NEXT-BODY ( -- ptr u8 n )
   0 VS-SKIPSTR !
   VS-NEXT ;

: VS-RAW! ( -- )
   VS-NEXT-RAW  VS-TU !  VS-TA ! ;

: VS-BODY! ( -- )
   VS-NEXT-BODY  VS-TU !  VS-TA ! ;

: VS-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   VS-L @ u + 1 + BODYBUF-CAP > IF s" verify-source: check body too long" 74 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  VS-BUF VS-L @ + c!
      VS-L @ 1 + VS-L !
      1 +
   REPEAT drop
   32 VS-BUF VS-L @ + c!  VS-L @ 1 + VS-L ! ;

: VS-LINE-COL ( n -- n n ) {: idx:n :}
   1 VS-LINE !
   1 VS-COL !
   0 VS-POS !
   BEGIN VS-POS @ idx <  VS-POS @ VS-U @ < and WHILE
      VS-SRC@ VS-POS @ + c@ 10 = IF
         VS-LINE @ 1 + VS-LINE !
         1 VS-COL !
      ELSE
         VS-COL @ 1 + VS-COL !
      THEN
      VS-POS @ 1 + VS-POS !
   REPEAT
   VS-LINE @ VS-COL @ ;

: VS-ABS-ORIGIN ( n -- n n n ) {: idx:n :}
   idx VS-LINE-COL {: line:n col:n :}
   VS-BASE-LINE @ line + 1 -
   line 1 = IF VS-BASE-COL @ col + 1 - ELSE col THEN
   VS-BASE-BYTE @ idx + ;

: VS-ORIGIN! ( n -- )
   VS-ABS-ORIGIN DIAG-ORIGIN! ;

: VS-MAYBE-SIG ( -- )
   VS-SKIP-WS
   VS-I @ VS-U @ < IF
      VS-SRC@ VS-I @ + c@ 40 = IF
         VS-I @ VS-START !
         41 VS-SKIP-PAST
         VS-FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
         VS-SRC@ VS-START @ +  VS-I @ VS-START @ -  VS-APP
      THEN
   THEN ;

: VS-REQUIRE-SIG ( -- ptr u8 n )
   VS-SKIP-WS
   VS-I @ VS-U @ >= IF s" verify-source: missing signature" 74 die THEN
   VS-SRC@ VS-I @ + c@ 40 <> IF s" verify-source: missing signature" 74 die THEN
   VS-I @ 1+ VS-START !
   41 VS-SKIP-PAST
   VS-FOUND @ 0= IF s" verify-source: unterminated signature" 74 die THEN
   VS-SRC@ VS-START @ + VS-I @ VS-START @ - 1 - ;

: VS-ENDS-Q? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > IF a u + 1 - c@ 34 = ELSE 0 0= 0= THEN ;

: VS-APP-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   a u VS-APP
   BEGIN
      VS-RAW!
      VS-TU @ 0= IF s" verify-source: unterminated string" 74 die THEN
      VS-TA @ VS-TU @ VS-ENDS-Q? VS-ENDQ !
      VS-TA @ VS-TU @ VS-APP
      VS-ENDQ @ IF EXIT THEN
   AGAIN ;

: VS-SKIP-STRING-REST ( ptr u8 n -- ) {: a:ptr u:n :}
   a u VS-ENDS-Q? IF exit THEN
   BEGIN
      VS-RAW!
      VS-TU @ 0= IF s" verify-source: unterminated string" 74 die THEN
      VS-TA @ VS-TU @ VS-ENDS-Q? IF EXIT THEN
   AGAIN ;

: VS-PARSE-NEXT? ( ptr u8 n -- bool )
   2dup s" char" CORE-STR= IF 2drop 0 0= exit THEN
   s" [char]" CORE-STR= ;

: VS-APP-NEXT-BODY ( -- )
   VS-BODY!
   VS-TU @ 0= IF s" verify-source: missing parsed token" 74 die THEN
   VS-TA @ VS-TU @ VS-APP ;

: VS-APP-BODY-TOKEN ( -- )
   VS-TA @ VS-TU @ VS-PARSE-NEXT? IF
      VS-TA @ VS-TU @ VS-APP
      VS-APP-NEXT-BODY
      exit
   THEN
   VS-TA @ VS-TU @ VS-OPN? IF
      VS-TA @ VS-TU @ VS-APP-STRING
   ELSE
      VS-TA @ VS-TU @ VS-APP
   THEN ;

: VS-SKIP-NEXT-BODY ( -- )
   VS-BODY!
   VS-TU @ 0= IF s" verify-source: missing parsed token" 74 die THEN ;

: VS-SKIP-BODY-TOKEN ( -- )
   VS-TA @ VS-TU @ VS-PARSE-NEXT? IF VS-SKIP-NEXT-BODY exit THEN
   VS-TA @ VS-TU @ VS-OPN? IF VS-TA @ VS-TU @ VS-SKIP-STRING-REST THEN ;

TRUSTED: VS-CHECK-BODY ( ptr u8 n -- n )
   CHECK! dup 1 = JSON-DIAGS @ 0= and DIAGXT @ 0 <> and IF DIAGXT @ execute THEN ;

: VS-VERIFY-BODY ( -- )
   VS-BUF VS-L @ VS-CHECK-BODY  dup -1 <> IF 70 throw THEN drop ;

TRUSTED: VS-CHECK-DOES-BODY ( ptr u8 n ptr u8 n -- n )
   CHECK-DOES! ;

: VS-VERIFY-DOES-BODY ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   VS-BUF VS-L @ sig sigu VS-CHECK-DOES-BODY
   dup -1 <> IF 70 throw THEN drop ;

: VS-VERIFY-DOES ( -- )
   VS-VERIFY-BODY
   VS-REQUIRE-SIG {: sig:ptr sigu:n :}
   0 VS-L !
   BEGIN
      VS-BODY!
      VS-TU @ 0= IF s" verify-source: unterminated does body" 74 die THEN
      VS-L @ 0= IF VS-START @ VS-ORIGIN! THEN
      VS-TA @ VS-TU @ s" ;" CORE-STR= IF sig sigu VS-VERIFY-DOES-BODY EXIT THEN
      VS-APP-BODY-TOKEN
   AGAIN ;

TRUSTED: VS-TRUST-SIG ( ptr u8 n ptr u8 n -- )
   TRUST ;

: VS-FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

: VS-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> IF 0 0= 0= EXIT THEN
   0 BEGIN dup u < WHILE
      dup a + c@ VS-FOLD-C
      over b + c@ VS-FOLD-C <> IF drop 0 0= 0= EXIT THEN
      1+
   REPEAT drop 0 0= ;

: VS-TRUST-NEXT ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   VS-NEXT-SCAN
   dup 0= IF s" verify-source: missing defining-word name" 74 die THEN
   sig sigu VS-TRUST-SIG ;

: VS-TRUST-DEFER-SIG ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu VS-REQUIRE-SIG VS-TRUST-SIG
   name nameu CHECKER-DEFER ;

: VS-TRUST-DEFER ( -- )
   VS-NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing defer name" 74 die THEN
   name nameu VS-TRUST-DEFER-SIG ;

: VS-SKIP-TRUSTED-BODY ( -- )
   BEGIN
      VS-BODY!
      VS-TU @ 0= IF s" verify-source: unterminated trusted definition" 74 die THEN
      VS-TA @ VS-TU @ s" ;" CORE-STR= IF EXIT THEN
      VS-SKIP-BODY-TOKEN
   AGAIN ;

: VS-TRUSTED ( -- )
   VS-NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing trusted name" 74 die THEN
   name nameu VS-REQUIRE-SIG VS-TRUST-SIG
   VS-SKIP-TRUSTED-BODY ;

: VS-UNDEFINE ( -- )
   VS-NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing undefine name" 74 die THEN
   name nameu CHECKER-UNDEFINE ;

: VS-PACKAGE ( -- )
   VS-NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing package name" 74 die THEN
   name nameu CHECKER-PACKAGE ;

: VS-PUBLIC ( -- )
   CHECKER-PUBLIC ;

: VS-PRIVATE ( -- )
   CHECKER-PRIVATE ;

: VS-END-PACKAGE ( -- )
   CHECKER-END-PACKAGE ;

: VS-DEFTYPE ( -- )
   VS-NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing deftype name" 74 die THEN
   name nameu CHECKER-DEFTYPE ;

: VS-DEFLINEAR ( -- )
   VS-NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing deflinear name" 74 die THEN
   name nameu CHECKER-DEFLINEAR ;

: VS-VREC-END? ( ptr u8 n -- bool )
   s" END-VALUE-RECORD" VS-STR=CI ;

: VS-VALUE-RECORD ( -- )
   VS-NEXT-SCAN {: name:ptr nameu:n :}
   nameu 0= IF s" verify-source: missing value-record name" 74 die THEN
   0 VS-L !
   BEGIN
      VS-NEXT-SCAN
      dup 0= IF s" verify-source: missing END-VALUE-RECORD" 74 die THEN
      2dup VS-VREC-END? IF
         2drop
         name nameu VS-BUF VS-L @ CHECKER-DEFRECORD
         EXIT
      THEN
      VS-APP
   AGAIN ;

: VS-RECORD-DEFINER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" VS-STR=CI IF VS-PACKAGE 0 0= EXIT THEN
   a u s" public" VS-STR=CI IF VS-PUBLIC 0 0= EXIT THEN
   a u s" private" VS-STR=CI IF VS-PRIVATE 0 0= EXIT THEN
   a u s" end-package" VS-STR=CI IF VS-END-PACKAGE 0 0= EXIT THEN
   a u s" deftype" VS-STR=CI IF VS-DEFTYPE 0 0= EXIT THEN
   a u s" deflinear" VS-STR=CI IF VS-DEFLINEAR 0 0= EXIT THEN
   a u s" value-record" VS-STR=CI IF VS-VALUE-RECORD 0 0= EXIT THEN
   a u s" constant" VS-STR=CI IF s" -- a" VS-TRUST-NEXT 0 0= EXIT THEN
   a u s" create" VS-STR=CI IF s" -- ptr a" VS-TRUST-NEXT 0 0= EXIT THEN
   a u s" variable" VS-STR=CI IF s" -- ptr a" VS-TRUST-NEXT 0 0= EXIT THEN
   a u s" defer" VS-STR=CI IF VS-TRUST-DEFER 0 0= EXIT THEN
   a u s" trusted:" VS-STR=CI IF VS-TRUSTED 0 0= EXIT THEN
   a u s" undefine" VS-STR=CI IF VS-UNDEFINE 0 0= EXIT THEN
   0 0= 0= ;

: VS-VERIFY-DEF ( -- )
   0 VS-L !
   VS-BODY!
   VS-TU @ 0= IF s" verify-source: missing word name" 74 die THEN
   VS-START @ VS-ORIGIN!
   VS-TA @ VS-TU @ VS-APP
   VS-MAYBE-SIG
   BEGIN
      VS-BODY!
      VS-TU @ 0= IF s" verify-source: unterminated definition" 74 die THEN
      VS-TA @ VS-TU @ s" ;" CORE-STR= IF VS-VERIFY-BODY EXIT THEN
      VS-TA @ VS-TU @ s" does>" CORE-STR= IF VS-VERIFY-DOES EXIT THEN
      VS-APP-BODY-TOKEN
   AGAIN ;

: VS-VERIFY-SOURCE ( -- )
   0 VS-I !
   BEGIN
      VS-NEXT-SCAN dup 0 > WHILE
      2dup s" :" CORE-STR= IF 2drop VS-VERIFY-DEF ELSE
      2dup VS-RECORD-DEFINER? IF 2drop ELSE 2drop THEN THEN
   REPEAT 2drop ;

: VERIFY-SOURCE-THROW ( n -- )
   dup 0= IF drop exit THEN
   throw ;

: VERIFY-SOURCE-RUN ( -- )
   [: VS-VERIFY-SOURCE ;] catch VERIFY-SOURCE-THROW ;

: VERIFY-SOURCE-BUF-IN-SCOPE ( ptr u8 n -- )
   VS-SRC!
   VERIFY-SOURCE-RUN ;

: VERIFY-SOURCE-BUF-AT-IN-SCOPE ( ptr u8 n n n n -- )
   VS-SRC-AT!
   VERIFY-SOURCE-RUN ;

: VERIFY-SOURCE-BUF ( ptr u8 n -- )
   VS-SRC!
   CHECKER-CANDIDATE-SCOPE-START
   [: VERIFY-SOURCE-RUN ;] catch
   CHECKER-CANDIDATE-SCOPE-DONE
   VERIFY-SOURCE-THROW ;

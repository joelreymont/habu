\ build.f — driver: bake a USER program into a standalone signed engine binary.
\ tools/hb-build.sh owns the I/O paths: it writes the bundled source to
\ /tmp/hb-build-src, the user-only check input to /tmp/hb-build-check-src, runs
\ the maker (toolchain + this driver, compiled by bin/hb), and moves
\ /tmp/hb-build-got to the requested output.
\ The toolchain compiling THIS driver is checker-hooked. This driver also
\ pre-verifies user colon definitions with CHECK! before bundling the source.
\ It does NOT execute top-level user code at build time; the emitted bundle still
\ recompiles/runs the full source at its own startup.

: BLD-IN  s" hb-build-src" TMP-PATH ;
: BLD-CHK s" hb-build-check-src" TMP-PATH ;
: BLD-OUT s" hb-build-got" TMP-PATH ;

variable PB  variable PN  variable PFD  variable PRD
$40000 constant PMAX

: ENSURE-PBUF
   PB @ 0= IF here PB !  PMAX allot THEN ;

: READ-PATH {: a u :}
   a u PATH0  0 0 open PFD !
   ENSURE-PBUF  0 PN !
   BEGIN                                                 \ read() may return short
     PFD @  PB @ PN @ +  PMAX PN @ -  read PRD !
     PRD @ 0 >
   WHILE  PN @ PRD @ + PN !  REPEAT
   PFD @ close
   PN @ 0 > 0= IF s" hb-build: empty source" 74 die THEN
   PN @ PMAX = IF s" hb-build: source exceeds buffer" 74 die THEN ;

: READ-CHECK  BLD-CHK READ-PATH ;
: READ-PROG   BLD-IN  READ-PATH ;

variable VI  variable VT  variable VSKIPSTR  variable VFOUND  variable VSTART
variable VTA  variable VTU  variable VENDQ
variable VL
create VBUF BODYBUF-CAP allot

: V-SKIP-WS
   BEGIN VI @ PN @ < IF PB @ VI @ + c@ 33 < ELSE 0 THEN WHILE
      VI @ 1 + VI !
   REPEAT ;

: V-SKIP-PAST {: ch :}
   0 VFOUND !
   BEGIN VI @ PN @ < WHILE
      PB @ VI @ + c@  VI @ 1 + VI !  ch = IF -1 VFOUND ! EXIT THEN
   REPEAT ;

: V-NEXT-RAW ( -- a u )
   V-SKIP-WS
   VI @ PN @ >= IF PB @ 0 EXIT THEN
   PB @ VI @ + VT !
   BEGIN VI @ PN @ < IF PB @ VI @ + c@ 32 > ELSE 0 THEN WHILE
      VI @ 1 + VI !
   REPEAT
   VT @  PB @ VI @ +  VT @ - ;

: V-OPN? {: a u :}
   u 2 = IF
      a 1 + c@ 34 = IF
         a c@ 115 =  a c@ 46 = or  a c@ 99 = or
      ELSE 0 THEN
   ELSE 0 THEN ;

: V-NEXT ( -- a u )
   BEGIN
      V-NEXT-RAW
      dup 0= IF EXIT THEN
      2dup 1 = swap c@ 92 = and IF 2drop 10 V-SKIP-PAST ELSE
      2dup 1 = swap c@ 40 = and IF 2drop 41 V-SKIP-PAST ELSE
      VSKIPSTR @ IF
         2dup V-OPN? IF 2drop 34 V-SKIP-PAST ELSE EXIT THEN
      ELSE EXIT THEN
      THEN THEN
   AGAIN ;

: V-NEXT-SCAN  -1 VSKIPSTR !  V-NEXT ;
: V-NEXT-BODY   0 VSKIPSTR !  V-NEXT ;
: V-RAW!   V-NEXT-RAW  VTU !  VTA ! ;
: V-BODY!  V-NEXT-BODY VTU !  VTA ! ;

: V-APP {: a u :}
   VL @ u + 1 + BODYBUF-CAP > IF s" hb-build: check body too long" 74 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  VBUF VL @ + c!
      VL @ 1 + VL !
      1 +
   REPEAT drop
   32 VBUF VL @ + c!  VL @ 1 + VL ! ;

: V-MAYBE-SIG
   V-SKIP-WS
   VI @ PN @ < IF
      PB @ VI @ + c@ 40 = IF
         PB @ VI @ + VSTART !
         41 V-SKIP-PAST
         VFOUND @ 0= IF s" hb-build: unterminated signature" 74 die THEN
         VSTART @  PB @ VI @ + VSTART @ -  V-APP
      THEN
   THEN ;

: V-ENDS-Q? {: a u :}
   u 0 > IF a u + 1 - c@ 34 = ELSE 0 THEN ;

: V-APP-STRING {: a u :}
   a u V-APP
   BEGIN
      V-RAW!
      VTU @ 0= IF s" hb-build: unterminated string" 74 die THEN
      VTA @ VTU @ V-ENDS-Q? VENDQ !
      VTA @ VTU @ V-APP
      VENDQ @ IF EXIT THEN
   AGAIN ;

: V-VERIFY-BODY
   VBUF VL @ CHECK!  dup 0= IF s" hb-build: check rejected" 70 die THEN drop ;

: V-VERIFY-DEF
   0 VL !
   V-BODY!
   VTU @ 0= IF s" hb-build: missing word name" 74 die THEN
   VTA @ VTU @ V-APP
   V-MAYBE-SIG
   BEGIN
      V-BODY!
      VTU @ 0= IF s" hb-build: unterminated definition" 74 die THEN
      VTA @ VTU @ s" ;" STR= IF V-VERIFY-BODY EXIT THEN
      VTA @ VTU @ V-OPN? IF VTA @ VTU @ V-APP-STRING ELSE VTA @ VTU @ V-APP THEN
   AGAIN ;

: VERIFY-SOURCE
   0 VI !
   BEGIN
      V-NEXT-SCAN dup 0 > WHILE
      2dup s" :" STR= IF 2drop V-VERIFY-DEF ELSE 2drop THEN
   REPEAT 2drop ;

: GO
   READ-CHECK
   VERIFY-SOURCE
   READ-PROG
   PB @ SHK-A !  PN @ SHK-U !  -1 SHAKE? !
   0 STDIN? !
   PB @ PN @ EMIT-FORTH
   BUILD-IMAGE
   s" hb-prog" SET-SIGID  CODESIG2
   BLD-OUT PATH0  1537 493 open  dup MBUF MLEN @ write drop  close ;
GO

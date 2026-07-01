\ roles.f — nominal scalar role conversions baked into hb.
\
\ The checker treats roles such as idx/len/fd as distinct nominal cell types.
\ Runtime representation is still one cell, so these conversion bodies are no-op
\ casts. Their effects are pinned by TRUST rows and covered by the engine gate.

: DEFTYPE ( -- )
   parse-name dup 0= IF s" deftype: missing name" 70 die THEN
   CHECKER-DEFTYPE ;

TRUSTED: >IDX ( n -- idx ) ;
TRUSTED: IDX>N ( idx -- n ) ;

TRUSTED: >LEN ( n -- len ) ;
TRUSTED: LEN>N ( len -- n ) ;

TRUSTED: >COUNT ( n -- count ) ;
TRUSTED: COUNT>N ( count -- n ) ;

TRUSTED: >OFF ( n -- off ) ;
TRUSTED: OFF>N ( off -- n ) ;

TRUSTED: >FD ( n -- fd ) ;
TRUSTED: FD>N ( fd -- n ) ;

TRUSTED: >RC ( n -- rc ) ;
TRUSTED: RC>N ( rc -- n ) ;

TRUSTED: >PID ( n -- pid ) ;
TRUSTED: PID>N ( pid -- n ) ;

TRUSTED: >MS ( n -- ms ) ;
TRUSTED: MS>N ( ms -- n ) ;

TRUSTED: >NS ( n -- ns ) ;
TRUSTED: NS>N ( ns -- n ) ;

TRUSTED: >TOK ( n -- tok ) ;
TRUSTED: TOK>N ( tok -- n ) ;

TRUSTED: >REG ( n -- reg ) ;
TRUSTED: REG>N ( reg -- n ) ;

TRUSTED: >LABEL ( n -- label ) ;
TRUSTED: LABEL>N ( label -- n ) ;

TRUSTED: >VA ( n -- va ) ;
TRUSTED: VA>N ( va -- n ) ;

TRUSTED: >SYMIDX ( n -- symidx ) ;
TRUSTED: SYMIDX>N ( symidx -- n ) ;

TRUSTED: >ASM ( n -- asm ) ;
TRUSTED: ASM>N ( asm -- n ) ;

TRUSTED: >IMG ( n -- img ) ;
TRUSTED: IMG>N ( img -- n ) ;

TRUSTED: >SNAP ( n -- snap ) ;
TRUSTED: SNAP>N ( snap -- n ) ;

: ASM-PHASE ( -- asm )
   0 >ASM ;

: ASM-DROP ( asm -- )
   ASM>N drop ;

: IMG-PHASE ( -- img )
   0 >IMG ;

: IMG-DROP ( img -- )
   IMG>N drop ;

: SNAP-PHASE ( -- snap )
   0 >SNAP ;

: SNAP-DROP ( snap -- )
   SNAP>N drop ;

: DEFLINEAR ( -- )
   parse-name dup 0= IF s" deflinear: missing name" 70 die THEN
   CHECKER-DEFLINEAR ;

$1000 constant VRDEF-CAP
create VRDEF-BUF VRDEF-CAP allot
variable VRDEF-U
variable VRDEF-I

: VRDEF-CLEAR ( -- )
   0 VRDEF-U ! ;

: VRDEF-ROOM ( n -- )
   VRDEF-U @ + VRDEF-CAP > IF
      s" value-record: field list too long" 70 die
   THEN ;

: VRDEF-C, ( n -- )
   1 VRDEF-ROOM
   VRDEF-BUF VRDEF-U @ + c!
   VRDEF-U @ 1 + VRDEF-U ! ;

: VRDEF-SPACE ( -- )
   VRDEF-U @ 0 > IF 32 VRDEF-C, THEN ;

: VRDEF-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   u VRDEF-ROOM
   0 VRDEF-I !
   BEGIN VRDEF-I @ u < WHILE
      a VRDEF-I @ + c@ VRDEF-C,
      VRDEF-I @ 1 + VRDEF-I !
   REPEAT ;

: VRDEF-TOKEN+ ( ptr u8 n -- )
   VRDEF-SPACE
   VRDEF-APP ;

: VRDEF-FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

: VRDEF-STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> IF 0 0= 0= EXIT THEN
   0 BEGIN dup u < WHILE
      dup a + c@ VRDEF-FOLD-C
      over b + c@ VRDEF-FOLD-C <> IF drop 0 0= 0= EXIT THEN
      1+
   REPEAT drop 0 0= ;

: VRDEF-END? ( ptr u8 n -- bool )
   s" END-VALUE-RECORD" VRDEF-STR=CI ;

: VALUE-RECORD ( -- )
   parse-name dup 0= IF
      s" value-record: missing name" 70 die
   THEN
   {: name:ptr nameu:n :}
   VRDEF-CLEAR
   BEGIN
      parse-name dup 0= IF
         s" value-record: missing END-VALUE-RECORD" 70 die
      THEN
      2dup VRDEF-END? IF
         2drop
         name nameu VRDEF-BUF VRDEF-U @ CHECKER-DEFRECORD
         EXIT
      THEN
      VRDEF-TOKEN+
   AGAIN ;

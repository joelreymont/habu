\ roles.f — nominal scalar role conversions baked into hb.
\
\ The checker treats roles such as idx/len/fd as distinct nominal cell types.
\ Runtime representation is still one cell, so a conversion is a retype and
\ nothing else: each one is declared with `CAST:`, which publishes an identity
\ word and costs nothing at a call site.

\ The DTC-* words build a nominal cell type's converter pair, >NAME ( n -- NAME )
\ and NAME>N ( NAME -- n ). A nominal is one cell, distinct from n and every
\ other nominal, with no widening. The builder takes the upper-case surface name
\ and the lowercase family tail separately; verify-source.f RECORD-DEFTYPE
\ registers their trust signatures.
$200 constant DTC-CAP
create DTC-BUF DTC-CAP allot
variable DTC-U
variable DTC-NAME-OFF
variable DTC-NAME-U
variable DTC-SIG-OFF
variable DTC-SIG-U

: DTC-CLEAR ( -- ) 0 DTC-U ! ;

\ Any source can call DTC+, so the length is compared with the room left: in a
\ sum, a length near the maximum cell wraps back under the capacity.
: DTC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < u DTC-CAP DTC-U @ - > or IF
      s" nominal: converter text too long" 70 die
   THEN
   0 BEGIN dup u < WHILE
      dup a + c@  DTC-BUF DTC-U @ + c!
      DTC-U @ 1 + DTC-U !  1+
   REPEAT drop ;

: DTC-NAME$ ( -- ptr u8 n )
   DTC-BUF DTC-NAME-OFF @ + DTC-NAME-U @ ;

: DTC-SIG$ ( -- ptr u8 n )
   DTC-BUF DTC-SIG-OFF @ + DTC-SIG-U @ ;

\ The buffer carries two spans and nothing else: DECL-SIGNATURE is handed
\ DTC-NAME$ and DTC-SIG$ separately, so no declaration text is built here.
: DTC-BEGIN ( -- )
   DTC-CLEAR
   DTC-U @ DTC-NAME-OFF ! ;

: DTC-NAME-END ( -- )
   DTC-U @ DTC-NAME-OFF @ - DTC-NAME-U !
   DTC-U @ DTC-SIG-OFF ! ;

: DTC-SIG-END ( -- )
   DTC-U @ DTC-SIG-OFF @ - DTC-SIG-U ! ;

: DTC-BUILD-IN ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   DTC-BEGIN
   s" >" DTC+ sa su DTC+ DTC-NAME-END
   s" n -- " DTC+ ta tu DTC+ DTC-SIG-END ;

: DTC-BUILD-OUT ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   DTC-BEGIN
   sa su DTC+ s" >N" DTC+ DTC-NAME-END
   ta tu DTC+ s"  -- n" DTC+ DTC-SIG-END ;

\ The role conversions are casts, not trusted declarations: checker.f
\ CAST-CERTIFY refuses a bad cast at its declaration, while
\ `TRUSTED: L ( n -- ptr u8 ) ;` records any shape and fails only at a caller
\ (`8 L c@` compiled and crashed).

\ DEFLINEAR / VALUE-RECORD are top-level-interpret-only: executing one parses
\ the live input and mutates the type registry, which their ( -- ) rows do not
\ express. UNSAFE-TOK? rejects the bare tokens in checked bodies before the usig
\ is consulted (DO-TOK1 order) and EXPORT refuses a qualified alias
\ (E-EXPORT-UNSAFE), so the usig adds no checked capability; it keeps the
\ definers checker-known so src/core/internal-mark.f leaves them executable at
\ top level.

CAST: >IDX ( n -- idx )
CAST: IDX>N ( idx -- n )

CAST: >LEN ( n -- len )
CAST: LEN>N ( len -- n )

CAST: >COUNT ( n -- count )
CAST: COUNT>N ( count -- n )

CAST: >OFF ( n -- off )
CAST: OFF>N ( off -- n )

CAST: >FD ( n -- fd )
CAST: FD>N ( fd -- n )

CAST: >RC ( n -- rc )
CAST: RC>N ( rc -- n )

CAST: >PID ( n -- pid )
CAST: PID>N ( pid -- n )

CAST: >MS ( n -- ms )
CAST: MS>N ( ms -- n )

CAST: >NS ( n -- ns )
CAST: NS>N ( ns -- n )

CAST: >TOK ( n -- tok )
CAST: TOK>N ( tok -- n )

CAST: >REG ( n -- reg )
CAST: REG>N ( reg -- n )

CAST: >LABEL ( n -- label )
CAST: LABEL>N ( label -- n )

CAST: >VA ( n -- va )
CAST: VA>N ( va -- n )

CAST: >SYMIDX ( n -- symidx )
CAST: SYMIDX>N ( symidx -- n )

CAST: >ASM ( n -- asm )
CAST: ASM>N ( asm -- n )

CAST: >IMG ( n -- img )
CAST: IMG>N ( img -- n )

CAST: >SNAP ( n -- snap )
CAST: SNAP>N ( snap -- n )

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
NOMINAL-PROVIDER

$1000 constant VRDEF-CAP
create VRDEF-BUF VRDEF-CAP allot
variable VRDEF-U
variable VRDEF-I

: VRDEF-CLEAR ( -- )
   0 VRDEF-U ! ;

\ Compared with the room left, like DTC+: VRDEF-APP takes any caller's length.
: VRDEF-ROOM ( n -- ) {: u:n :}
   u 0 < u VRDEF-CAP VRDEF-U @ - > or IF
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
NOMINAL-PROVIDER

\ Seal the aliasable-unsafe words (DEFLINEAR/VALUE-RECORD here and the sumtype.f
\ and type-family.f openers) into the checker's identity set so EXPORT rejects
\ re-exporting any of them by symbol, not only by canonical spelling. Every
\ definer is interned by now, so each name resolves to its permanent symbol.
UNSAFE-SET-SEAL

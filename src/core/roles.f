\ roles.f — nominal scalar role conversions baked into hb.
\
\ The checker treats roles such as idx/len/fd as distinct nominal cell types.
\ Runtime representation is still one cell, so these conversion bodies are no-op
\ casts. Their effects are pinned by TRUST rows and covered by the engine gate.

\ The DTC-* words below build a nominal cell type's no-op converter pair —
\ >NAME ( n -- NAME ) and NAME>N ( NAME -- n ), matching the built-in role
\ pattern (>IDX / IDX>N). A nominal is one cell wide, distinct from n and from
\ every other nominal, with no widening; the converters are the only way across
\ the boundary and the checker never collapses a nominal to n. The builder takes
\ the UPPER-CASE surface name (the converter word spelling) and the lowercase
\ family tail (the signature type) separately; the checker's source recorder
\ (verify-source.f RECORD-NOMINAL) registers their trust signatures.
$200 constant DTC-CAP
create DTC-BUF DTC-CAP allot
variable DTC-U
variable DTC-NAME-OFF
variable DTC-NAME-U
variable DTC-SIG-OFF
variable DTC-SIG-U

: DTC-CLEAR ( -- ) 0 DTC-U ! ;

: DTC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DTC-U @ u + DTC-CAP > IF s" nominal: converter text too long" 70 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@  DTC-BUF DTC-U @ + c!
      DTC-U @ 1 + DTC-U !  1+
   REPEAT drop ;

: DTC-NAME$ ( -- ptr u8 n )
   DTC-BUF DTC-NAME-OFF @ + DTC-NAME-U @ ;

: DTC-SIG$ ( -- ptr u8 n )
   DTC-BUF DTC-SIG-OFF @ + DTC-SIG-U @ ;

: DTC-BEGIN ( -- )
   DTC-CLEAR
   s" TRUSTED: " DTC+
   DTC-U @ DTC-NAME-OFF ! ;

: DTC-NAME-END ( -- )
   DTC-U @ DTC-NAME-OFF @ - DTC-NAME-U !
   s"  ( " DTC+
   DTC-U @ DTC-SIG-OFF ! ;

: DTC-SIG-END ( -- )
   DTC-U @ DTC-SIG-OFF @ - DTC-SIG-U !
   s"  ) ;" DTC+ ;

: DTC-BUILD-IN ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   DTC-BEGIN
   s" >" DTC+ sa su DTC+ DTC-NAME-END
   s" n -- " DTC+ ta tu DTC+ DTC-SIG-END ;

: DTC-BUILD-OUT ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n ta:ptr tu:n :}
   DTC-BEGIN
   sa su DTC+ s" >N" DTC+ DTC-NAME-END
   ta tu DTC+ s"  -- n" DTC+ DTC-SIG-END ;

\ DEFLINEAR / VALUE-RECORD are top-level-interpret-only, like the sumtype.f
\ block openers: executing one parses the live input stream and mutates the
\ type registry — side effects their ( -- ) rows do not express. Unlike the
\ openers (PRIM: axioms) these are checked words, so certifying this file
\ records a usig per definer. UNSAFE-TOK? rejects the BARE tokens inside
\ checked bodies BEFORE the usig is consulted (DO-TOK1 order), and EXPORT
\ refuses to mint a qualified alias for these names (E-EXPORT-UNSAFE), so no
\ spelling reaches the usig from a checked body. The usig therefore adds no
\ checked-code capability — it only keeps the definers checker-known so the
\ seal-time internal-word marking pass (src/core/internal-mark.f) leaves
\ them executable at top level (LAYOUT-BUFFER parity, dots
\ habu-checker-deftype-deflinear-8e9d1dc5,
\ habu-checker-unsafety-must-d12bc784).

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

\ --- CAST: checked nominal/family retype declarer (dot habu-checked-cast-primitive).
\ `CAST: NAME ( in -- out ) <body> ;` compiles ONE ordinary checked colon word.
\ The checker (checker.f CAST-PEND window) certifies <body> under the identity
\ row ( in -- in ) and publishes the declared ( in -- out ), so the retype is
\ CHECKED, not trusted — this is the converter form that ends per-declaration
\ TRUSTED growth. NAME + signature + body are captured from the live input token
\ by token (parse-name is the only source primitive) and rebuilt into a ": NAME
\ ( in -- out ) <body> ;" definition; cast bodies are ordinary token code (a guard
\ that throws, never a raw string literal), so whitespace-joined tokens reproduce
\ them faithfully. The generated text never contains TRUST/TRUSTED:/set-check.
\ Top-level-interpret-only like the sibling definers: UNSAFE-TOK? rejects `cast:`
\ inside checked bodies and EXPORT refuses to alias it (checker.f), so it cannot
\ be laundered; a certified ( -- ) usig keeps it top-level executable past the
\ seal-time internal-word marking pass.
$400 constant CAST-GEN-CAP
create CAST-GEN CAST-GEN-CAP allot
variable CAST-GEN-U
variable CAST-GEN-I
variable CAST-NAME-OFF
variable CAST-NAME-U

: CAST-GEN-CLEAR ( -- ) 0 CAST-GEN-U ! ;
: CAST-GEN-C, ( n -- ) {: c:n :}
   CAST-GEN-U @ CAST-GEN-CAP >= IF s" cast: generated text too long" 70 die THEN
   c CAST-GEN CAST-GEN-U @ + c!
   CAST-GEN-U @ 1 + CAST-GEN-U ! ;
: CAST-GEN-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 CAST-GEN-I !
   BEGIN CAST-GEN-I @ u < WHILE
      a CAST-GEN-I @ + c@ CAST-GEN-C,
      CAST-GEN-I @ 1 + CAST-GEN-I !
   REPEAT ;

: CAST-NAME, ( ptr u8 n -- )   \ copy the name into the gen buffer, record its span
   CAST-GEN-U @ CAST-NAME-OFF !
   dup CAST-NAME-U !
   CAST-GEN-APP ;
: CAST-NAME$ ( -- ptr u8 n )
   CAST-GEN CAST-NAME-OFF @ + CAST-NAME-U @ ;

: CAST-CAPTURE-BODY ( -- )   \ append remaining live tokens through ';' (space-joined)
   BEGIN
      parse-name dup 0= IF s" cast: missing ;" 70 die THEN
      2dup s" ;" CORE-STR= IF s" ;" CAST-GEN-APP 2drop EXIT THEN
      CAST-GEN-APP 32 CAST-GEN-C,
   AGAIN ;

: CAST-EVAL-RUN ( -- ) CAST-GEN CAST-GEN-U @ TDECL-EVAL-XT ;
: CAST-EVAL ( -- )
   CAST-NAME$ CAST-PEND!                 \ arm the one-shot cast-certification window
   [: CAST-EVAL-RUN ;] catch
   CAST-NAME$ drop 0 CAST-PEND!          \ disarm (zero-length name) on every exit path
   dup 0 <> IF throw THEN drop ;         \ re-raise a compile/legality reject to the caller

: CAST: ( -- )
   parse-name dup 0= IF s" cast: missing name" 70 die THEN {: name:ptr nameu:n :}
   CAST-GEN-CLEAR
   s" : " CAST-GEN-APP
   name nameu CAST-NAME,
   32 CAST-GEN-C,
   parse-name dup 0= IF s" cast: missing ( in -- out ) signature" 70 die THEN
   2dup s" (" CORE-STR= 0= IF s" cast: signature must open with (" 70 die THEN
   CAST-GEN-APP 32 CAST-GEN-C,
   CAST-CAPTURE-BODY
   CAST-EVAL ;

\ Seal the aliasable-unsafe words (this file's DEFLINEAR/VALUE-RECORD and
\ the earlier sumtype.f/type-family.f openers) into the checker's identity set so
\ EXPORT rejects re-exporting any of them BY SYMBOL, not just by their canonical
\ spelling (dot habu-checker-unsafety-as-1c537c1f). Every definer/opener is
\ interned by now, so each name resolves to its permanent symbol.
UNSAFE-SET-SEAL

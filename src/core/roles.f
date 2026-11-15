\ roles.f — nominal scalar role conversions baked into hb.
\
\ The checker treats roles such as idx/len/fd as distinct nominal cell types.
\ Runtime representation is still one cell, so a conversion is a retype and
\ nothing else: each one is declared with `CAST:`, which publishes an identity
\ word and costs nothing at a call site.

package ROLES

EXPORT CHECKER-REG:SCRATCH{
EXPORT CHECKER-REG:}SCRATCH

\ The DTC-* words give the name and effect text of a nominal cell type's
\ converter pair, >NAME ( n -- NAME ) and NAME>N ( NAME -- n ), for
\ verify-source.f's definition report. A nominal is one cell, distinct from n
\ and every other nominal, with no widening. The builder takes the upper-case
\ surface name, the lowercase family tail and the base separately; a nominal
\ over a base has >NAME ( base -- NAME ) and NAME>BASE ( NAME -- base ).

\ The table grows: a DEFTYPE name may be as long as any family name, and any
\ source can call DTC+. Its cells are scratch, so an image holds none.
$200 constant DTC-INIT
SCRATCH{
PTR-VARIABLE DTC-P   variable DTC-U   variable DTC-CAP
}SCRATCH
variable DTC-NAME-OFF
variable DTC-NAME-U
variable DTC-SIG-OFF
variable DTC-SIG-U

: DTC-CLEAR ( -- ) 0 DTC-U ! ;

: DTC-BUF ( -- ^u8 ) DTC-P @ ;

EXPORT CHECK:CHECKER-DEFLINEAR
EXPORT CHECK:CHECKER-DEFRECORD
EXPORT CHECK:NOMINAL-PROVIDER
EXPORT CHECK:SR-GROW
: DTC+ ( ^u8 n -- ) {: a:^ u:n :}
   DTC-U @ u + DTC-CAP @ > IF
      DTC-U @ u + DTC-CAP @ 2 * max DTC-INIT max {: nc:n :}
      DTC-P @ DTC-CAP @ nc SR-GROW DTC-P !
      nc DTC-CAP !
   THEN
   a DTC-BUF DTC-U @ + u ARENA-COPY
   DTC-U @ u + DTC-U ! ;

: DTC-NAME$ ( -- ^u8 n )
   DTC-BUF DTC-NAME-OFF @ + DTC-NAME-U @ ;

: DTC-SIG$ ( -- ^u8 n )
   DTC-BUF DTC-SIG-OFF @ + DTC-SIG-U @ ;

\ The buffer carries two spans and nothing else: DTC-NAME$ and DTC-SIG$ are
\ read separately, so no declaration text is built here.
: DTC-BEGIN ( -- )
   DTC-CLEAR
   DTC-U @ DTC-NAME-OFF ! ;

: DTC-NAME-END ( -- )
   DTC-U @ DTC-NAME-OFF @ - DTC-NAME-U !
   DTC-U @ DTC-SIG-OFF ! ;

: DTC-SIG-END ( -- )
   DTC-U @ DTC-SIG-OFF @ - DTC-SIG-U ! ;

\ DTC+, with each byte a-z in upper case.
: DTC-UPPER+ ( ^u8 n -- ) {: a:^ u:n :}
   a u DTC+
   DTC-BUF DTC-U @ u - + {: at:^ :}
   0 BEGIN dup u < WHILE
      dup at + c@ dup 97 < over 122 > or 0= IF 32 - THEN  over at + c!
      1+
   REPEAT drop ;

\ >NAME ( base -- tail ) of surface SA SU, tail TA TU and base BA BU, which is
\ `n` for a one-cell nominal.
: DTC-BUILD-IN ( ^u8 n ^u8 n ^u8 n -- ) {: sa:^ su:n ta:^ tu:n ba:^ bu:n :}
   DTC-BEGIN
   s" >" DTC+ sa su DTC+ DTC-NAME-END
   ba bu DTC+ s"  -- " DTC+ ta tu DTC+ DTC-SIG-END ;

\ NAME>BASE ( tail -- base ), the base in upper case in the name: NAME>N for `n`.
: DTC-BUILD-OUT ( ^u8 n ^u8 n ^u8 n -- ) {: sa:^ su:n ta:^ tu:n ba:^ bu:n :}
   DTC-BEGIN
   sa su DTC+ s" >" DTC+ ba bu DTC-UPPER+ DTC-NAME-END
   ta tu DTC+ s"  -- " DTC+ ba bu DTC+ DTC-SIG-END ;

\ The role conversions are casts, not trusted declarations: checker.f
\ CAST-CERTIFY refuses a bad cast at its declaration, while
\ `TRUSTED: L ( n -- ^u8 ) ;` records any shape and fails only at a caller
\ (`8 L c@` compiled and crashed).

\ DEFLINEAR / VALUE-RECORD are top-level-interpret-only: executing one parses
\ the live input and mutates the type registry, which their ( -- ) rows do not
\ express. UNSAFE-TOK? rejects the bare tokens in checked bodies before the usig
\ is consulted (DO-TOK1 order), and a global section's word has no qualified
\ spelling, so the usig adds no checked capability; it keeps the definers
\ checker-known, so top level may run them.

global
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

\ The definer just read has no name after it.
: DEF-NO-NAME ( -- )
   SRCLOC:TOKEN$ SRCLOC:TOKEN$ s" no name follows" REJECTION:MISSING-NAME
   CHECKER-REJECT-RC REJECTION:STOP ;

: DEFLINEAR ( -- )
   SRCLOC:TOKEN$ {: kw:^ ku:n :}
   parse-name dup 0= IF DEF-NO-NAME THEN
   {: name:^ nameu:n :}
   kw ku name nameu REJECTION:NOT-A-NAME
   name nameu CHECKER-DEFLINEAR ;
NOMINAL-PROVIDER
private

: VRDEF-FOLD-C ( n -- n )
   dup $41 < IF EXIT THEN
   dup $5A > IF EXIT THEN
   $20 or ;

: VRDEF-STR=CI ( ^u8 n ^u8 n -- bool ) {: a:^ u:n b:^ v:n :}
   u v <> IF 0 0= 0= EXIT THEN
   0 BEGIN dup u < WHILE
      dup a + c@ VRDEF-FOLD-C
      over b + c@ VRDEF-FOLD-C <> IF drop 0 0= 0= EXIT THEN
      1+
   REPEAT drop 0 0= ;

: VRDEF-END? ( ^u8 n -- bool )
   s" END-VALUE-RECORD" VRDEF-STR=CI ;

\ The source ends inside value record a u. It is written through its help, and
\ the caller stops it.
: VRDEF-UNCLOSED ( ^u8 n -- ) {: a:^ u:n :}
   s" E-BAD-DECLARATION" REJECTION:OPEN
   s" value record `" REJECTION:EMIT-STR  a u REJECTION:EMIT-STR
   s" ` has no `END-VALUE-RECORD`" REJECTION:EMIT-STR
   a u REJECTION:AT  s" source ends first" REJECTION:EMIT-STR
   REJECTION:HELP  s" end the field list with `END-VALUE-RECORD`" REJECTION:EMIT-STR ;

global
\ The field list is the source from its first token f to the closer at e:
\ parse-name never refills the input, so the span is the record's own bytes and
\ a rejected field is placed.
: VALUE-RECORD ( -- )
   SRCLOC:TOKEN$ {: kw:^ ku:n :}
   parse-name dup 0= IF DEF-NO-NAME THEN
   {: name:^ nameu:n :}
   kw ku name nameu REJECTION:NOT-A-NAME
   parse-name over {: f:^ :}
   BEGIN
      dup 0= IF
         name nameu VRDEF-UNCLOSED  CHECKER-REJECT-RC REJECTION:STOP
      THEN
      2dup VRDEF-END? IF
         drop {: e:^ :}  name nameu f e f - CHECKER-DEFRECORD
         EXIT
      THEN
      2drop parse-name
   AGAIN ;
NOMINAL-PROVIDER
private
;package

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
EXPORT CHECK:DEF-NAME-REJECT?
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-DECLARATION
EXPORT CHECK:NOMINAL-PROVIDER
EXPORT CHECK:SR-GROW
: DTC+ ( str -- ) {: s:str :}
   DTC-U @ s STR:LEN + DTC-CAP @ > IF
      DTC-U @ s STR:LEN + DTC-CAP @ 2 * max DTC-INIT max {: nc:n :}
      DTC-P @ DTC-CAP @ nc SR-GROW DTC-P !
      nc DTC-CAP !
   THEN
   s STR:DATA  DTC-BUF DTC-U @ + s STR:LEN >str  ARENA-COPY
   DTC-U @ s STR:LEN + DTC-U ! ;

: DTC-NAME$ ( -- str )
   DTC-BUF DTC-NAME-OFF @ + DTC-NAME-U @ >str ;

: DTC-SIG$ ( -- str )
   DTC-BUF DTC-SIG-OFF @ + DTC-SIG-U @ >str ;

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
: DTC-UPPER+ ( str -- ) {: s:str :}
   s DTC+
   DTC-BUF DTC-U @ s STR:LEN - + {: at:^ :}
   0 BEGIN dup s STR:LEN < WHILE
      dup at + c@ dup 97 < over 122 > or 0= IF 32 - THEN  over at + c!
      1+
   REPEAT drop ;

\ >NAME ( base -- tail ) of surface SS, tail TS and base BS, which is
\ `n` for a one-cell nominal.
: DTC-BUILD-IN ( str str str -- ) {: ss:str ts:str bs:str :}
   DTC-BEGIN
   s" >" DTC+ ss DTC+ DTC-NAME-END
   bs DTC+ s"  -- " DTC+ ts DTC+ DTC-SIG-END ;

\ NAME>BASE ( tail -- base ), the base in upper case in the name: NAME>N for `n`.
: DTC-BUILD-OUT ( str str str -- ) {: ss:str ts:str bs:str :}
   DTC-BEGIN
   ss DTC+ s" >" DTC+ bs DTC-UPPER+ DTC-NAME-END
   ts DTC+ s"  -- " DTC+ bs DTC+ DTC-SIG-END ;

\ The role conversions are casts, not trusted declarations: a bad cast is
\ rejected at its declaration, while `TRUSTED: L ( n -- ^u8 ) ;` records any
\ shape and fails only at a caller.

\ DEFLINEAR / VALUE-RECORD are top-level-interpret-only: executing one parses
\ the live input and mutates the type registry, which their ( -- ) rows do not
\ express. Their declaration id bars them from checked bodies, qualified or
\ not; the usig keeps the definers checker-known, so top level may run them.

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
   SRCLOC:TOKEN$ >str SRCLOC:TOKEN$ >str s" no name follows"
   REJECTION:MISSING-NAME
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: DEFLINEAR ( -- )
   SRCLOC:TOKEN$ >str {: kw:str :}
   parse-name dup STR:LEN 0= IF DEF-NO-NAME THEN
   {: name:str :}
   kw name REJECTION:NOT-A-NAME
   name name DEF-NAME-REJECT? IF EXIT-STATUS:REJECTED REJECTION:STOP THEN
   name dup STR:DATA swap STR:LEN CHECKER-DEFLINEAR ;
NOMINAL-PROVIDER
INTRINSIC-DECLARATION INTRINSIC
private

: VRDEF-END? ( str -- bool )
   s" END-VALUE-RECORD" STR:=CI ;

\ The source ends inside value record s. It is written through its help, and
\ the caller stops it.
: VRDEF-UNCLOSED ( str -- ) {: s:str :}
   s" E-BAD-DECLARATION" REJECTION:OPEN
   s" value record `" REJECTION:EMIT-STR  s REJECTION:EMIT-STR
   s" ` has no `END-VALUE-RECORD`" REJECTION:EMIT-STR
   s REJECTION:AT  s" source ends first" REJECTION:EMIT-STR
   REJECTION:HELP  s" end the field list with `END-VALUE-RECORD`" REJECTION:EMIT-STR ;

global
\ The field list is the source from its first token f to the closer at e:
\ parse-name never refills the input, so the span is the record's own bytes and
\ a rejected field is placed.
: VALUE-RECORD ( -- )
   SRCLOC:TOKEN$ >str {: kw:str :}
   parse-name dup STR:LEN 0= IF DEF-NO-NAME THEN
   {: name:str :}
   kw name REJECTION:NOT-A-NAME
   name name DEF-NAME-REJECT? IF EXIT-STATUS:REJECTED REJECTION:STOP THEN
   parse-name dup STR:DATA {: f:^ :}
   BEGIN
      dup STR:LEN 0= IF
         name VRDEF-UNCLOSED  EXIT-STATUS:REJECTED REJECTION:STOP
      THEN
      dup VRDEF-END? IF
         STR:DATA {: e:^ :}
         name dup STR:DATA swap STR:LEN  f e f - CHECKER-DEFRECORD
         EXIT
      THEN
      drop parse-name
   AGAIN ;
NOMINAL-PROVIDER
INTRINSIC-DECLARATION INTRINSIC
private
;package

\ arm64-place.f - LINKER's ARM64:PLACE over a fixture word's arm64 shadow
\ emission. Placed at the host routine's entry it matches the routine T0 placed
\ there, code address chains aside; placed elsewhere its calls and chains reach
\ what the shadow named. The fixture is test/compiler/arm64-place-fixture.f,
\ loaded through the typed interpret loop at tier 0 while the shadow is open.

require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/lib/fs.f
require src/lib/test.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/interpret.f
require src/arch/arm64/asm.f
require src/habu/address-carrier.f
require src/arch/arm64/link.f

package LINKER
private
package ARM64-PLACE

CAST: CODE>BYTES ( n -- ptr u8 )

4096 constant SRC-CAP
SRC-CAP BUFFER: SRC                  \ the fixture's text
variable SRC-U
variable RECS0                       \ shadow records filed before the load
4096 constant OUT-CAP
OUT-CAP BUFFER: PLACED               \ a placed copy of the emission
16 BUFFER: SCRATCH                   \ one call site or code address chain

\ How a word of a placed copy may differ from the shadow's.
0 constant PLAIN
1 constant CALL-WORD
2 constant CODE-CHAIN
3 constant DATA-CHAIN

$123450 constant MOVE-BY             \ the second base's distance from the host's
$10000 constant LINK-BY              \ what ELSEWHERE's resolve and delta add


: T0? ( -- bool )
   data-base NCOMP-DISPATCH:TIER-CELL + @ 0=
   data-base NCOMP-DISPATCH:COMP-CELL + @ 0<> and ;


: TIER-CK ( -- )
   T0? 0= if
      s" arm64-place: the tier is not T0 (TIER-CELL 0 and COMP-CELL set)" 1 die
   then ;


: LOAD-FIXTURE ( -- )
   s" test/compiler/arm64-place-fixture.f" SRC SRC-CAP FS:READ-ALL SRC-U !
   SRC SRC-U @ 0 SRC-U @ OUTER:EVAL-FRAMED-RANGE ;


\ The last shadow row filed since the open whose record is named a u, or -1.
: ROW-NAMED ( ptr u8 n -- n )
   {: a:ptr u:n :}
   -1
   NSHADOW:RECORDS RECS0 @ ?do
      i NSHADOW:RECORD@ XREF-REC XREF-NAME$ a u STR= if drop i then
   loop ;


: SHADOW-AT ( n n -- ptr u8 ) {: e:n off:n :} e NSHADOW:BYTES off + ;

: SHADOW-END ( n -- ptr u8 ) {: e:n :} e NSHADOW:BYTES e NSHADOW:SIZE + ;

\ Every arm64 code address row is a 16-byte chain.
: CODE-CHAIN? ( n n -- bool ) NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-CODE = ;
: DATA-ROW? ( n n -- bool ) NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = ;


: CALLS-OF ( n n -- n )
   {: e:n kind:n :}
   0 e NSHADOW:CALL-SITES 0 ?do e i NSHADOW:CALL-KIND@ kind = if 1+ then loop ;


: DATA-ROWS ( n -- n )
   {: e:n :}
   0 e NSHADOW:ADDR-SITES 0 ?do e i DATA-ROW? if 1+ then loop ;


: CODE-CHAINS ( n -- n )
   {: e:n :}
   0 e NSHADOW:ADDR-SITES 0 ?do e i CODE-CHAIN? if 1+ then loop ;


: DATA-SIZE ( n n -- n )
   {: e:n k:n :}
   e e k NSHADOW:ADDR-SITE@ SHADOW-AT  e SHADOW-END  ADDRESS-CARRIER:CHAIN-SIZE ;


\ What emission e holds at byte off: a call row's word, a word inside a code
\ address chain or a data address chain, or a plain word.
: WORD-AT ( n n -- n )
   {: e:n off:n :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ off = if CALL-WORD unloop exit then
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: s:n :}
      off s >= if
         e i CODE-CHAIN? off s 16 + < and if CODE-CHAIN unloop exit then
         e i DATA-ROW? off s e i DATA-SIZE + < and if DATA-CHAIN unloop exit then
      then
   loop
   PLAIN ;


\ The adrp's immhi:immlo field, sign-extended: a page count.
: ADRP-PAGES ( n -- n )
   {: w:n :}
   w 29 rshift 3 and  w 5 rshift $7FFFF and 2 lshift or
   dup $100000 and 0<> if $200000 - then ;


\ Whether the chain at p, run at pc, is adrp rd / add rd, rd, #lo / nop / nop
\ giving want, per the ARM ARM: adrp's page is pc's 4 KiB page plus its field.
: CHAIN-GIVES? ( ptr u8 n n -- bool )
   {: p:ptr pc:n want:n :}
   p LE:U32@ {: adrp:n :}
   p 4 + LE:U32@ {: add:n :}
   adrp $1F and {: rd:n :}
   adrp $9F000000 and $90000000 =
   add $FFC00000 and $91000000 = and
   add $1F and rd = and  add 5 rshift $1F and rd = and
   p 8 + LE:U32@ A64ASM:ENC-NOP = and
   p 12 + LE:U32@ A64ASM:ENC-NOP = and
   pc $FFF invert and  adrp ADRP-PAGES 12 lshift +  add 10 rshift $FFF and +
   want = and ;


\ Every code address chain of the copy placed at at gives the shadow's value
\ plus by; the first site that does not, or -1.
: CHAINS-BAD ( n n n -- n )
   {: e:n at:n by:n :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i CODE-CHAIN? if
         e i NSHADOW:ADDR-SITE@ {: s:n :}
         PLACED s +  at s +
         e s SHADOW-AT ADDRESS-CARRIER:ADDR-CHAIN-BYTES ADDRESS-CARRIER:CHAIN-VALUE
         by +  CHAIN-GIVES? 0= if s unloop exit then
      then
   loop
   -1 ;


\ The first byte offset below n where a word of the copy outside its code address
\ chains differs from the host routine at h, or -1.
: HOST-DIFF ( n n n -- n )
   {: e:n h:n n:n :}
   n 4 / 0 ?do
      e i 4 * WORD-AT CODE-CHAIN <> if
         PLACED i 4 * + LE:U32@  h i 4 * + CODE>BYTES LE:U32@ <> if
            i 4 * unloop exit
         then
      then
   loop
   -1 ;


\ The call word at p, run at pc: its bl/b target, or -1 for another opcode.
: CALL-TO ( ptr u8 n n -- n )
   {: p:ptr pc:n kind:n :}
   p LE:U32@ {: w:n :}
   kind NEMIT:CALL = if $94000000 else $14000000 then {: op:n :}
   w $FC000000 and op <> if -1 exit then
   w $3FFFFFF and  dup $2000000 and 0<> if $4000000 - then  4 *  pc + ;


\ The first call row of the copy placed at at that does not reach its target
\ plus by, or -1.
: CALLS-BAD ( n n n -- n )
   {: e:n at:n by:n :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      PLACED s +  at s +  e i NSHADOW:CALL-KIND@ CALL-TO
      e i NSHADOW:CALL-TARGET@ by + <> if s unloop exit then
   loop
   -1 ;


\ The first data address row of the copy whose chain does not hold the shadow's
\ value plus by at the shadow's size, or -1.
: DATA-BAD ( n n -- n )
   {: e:n by:n :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i DATA-ROW? if
         e i NSHADOW:ADDR-SITE@ {: s:n :}
         e i DATA-SIZE {: size:n :}
         PLACED s +  PLACED e NSHADOW:SIZE +  ADDRESS-CARRIER:CHAIN-SIZE size <>
         PLACED s + size ADDRESS-CARRIER:CHAIN-VALUE
         e s SHADOW-AT size ADDRESS-CARRIER:CHAIN-VALUE by + <> or
         if s unloop exit then
      then
   loop
   -1 ;


\ The first plain word of the copy that differs from the shadow's, or -1.
: PLAIN-DIFF ( n -- n )
   {: e:n :}
   e NSHADOW:SIZE 4 / 0 ?do
      e i 4 * WORD-AT PLAIN = if
         PLACED i 4 * + LE:U32@  e i 4 * SHADOW-AT LE:U32@ <> if
            i 4 * unloop exit
         then
      then
   loop
   -1 ;


\ Emission e copied to PLACED to run at at, data moved by delta.
: PLACE-AT ( n n n [ n -- n ] -- )
   {: e:n at:n delta:n resolve :}
   e NSHADOW:SIZE OUT-CAP > if s" arm64-place: the emission outgrows PLACED" 1 die then
   e PLACED at delta resolve ARM64:PLACE ;


: KINDS ( n -- )
   {: e:n :}
   s" calls " type e NEMIT:CALL CALLS-OF .
   s" tail calls " type e NEMIT:TAIL CALLS-OF .
   s" data addresses " type e DATA-ROWS .
   s" code address chains " type e CODE-CHAINS . cr
   s" a call row" T-LABEL  e NEMIT:CALL CALLS-OF 0 > TTRUE
   s" a tail call row" T-LABEL  e NEMIT:TAIL CALLS-OF 0 > TTRUE
   s" a data address row" T-LABEL  e DATA-ROWS 0 > TTRUE
   s" a code address chain" T-LABEL  e CODE-CHAINS 0 > TTRUE ;


\ At the host routine's own entry h the copy is T0's placement of it.
: AT-HOST ( n n n -- )
   {: e:n h:n hlen:n :}
   e h 0 [: 0 + ;] PLACE-AT
   s" placed at the host entry: same words" T-LABEL
   e h  hlen e NSHADOW:SIZE min  HOST-DIFF -1 T=
   s" placed at the host entry: code address chains" T-LABEL
   e h 0 CHAINS-BAD -1 T= ;


\ At at with every target and data address moved by LINK-BY.
: ELSEWHERE ( n n -- )
   {: e:n at:n :}
   e at LINK-BY [: LINK-BY + ;] PLACE-AT
   s" placed elsewhere: calls" T-LABEL  e at LINK-BY CALLS-BAD -1 T=
   s" placed elsewhere: code address chains" T-LABEL
   e at LINK-BY CHAINS-BAD -1 T=
   s" placed elsewhere: data addresses" T-LABEL  e LINK-BY DATA-BAD -1 T=
   s" placed elsewhere: other words" T-LABEL  e PLAIN-DIFF -1 T= ;


: ENCODINGS ( -- )
   s" adrp x1, -5 pages" T-LABEL  1 -5 A64ASM:ENC-ADRP $F0FFFFC1 T=
   s" adr x1, -5" T-LABEL  1 -5 A64ASM:ENC-ADR $70FFFFC1 T= ;


\ to at: a bl from at to to written into SCRATCH.
: BL-AT ( n n -- n ) NEMIT:CALL swap SCRATCH ARM64:CALL! SCRATCH LE:U32@ ;


: REACH ( -- )
   s" bl 2^27 bytes on" T-LABEL
   [: $8000000 0 BL-AT drop ;] E-A64EMIT-REACH TTHROWSQ
   s" bl 2^27 - 4 bytes on" T-LABEL  $8000000 4 - 0 BL-AT $95FFFFFF T=
   s" bl 2^27 bytes back" T-LABEL  0 $8000000 - 0 BL-AT $96000000 T=
   s" bl 2^27 + 4 bytes back" T-LABEL
   [: 0 $8000000 - 4 - 0 BL-AT drop ;] E-A64EMIT-REACH TTHROWSQ
   s" bl to a byte between words" T-LABEL
   [: 6 0 BL-AT drop ;] E-A64EMIT-REACH TTHROWSQ
   0 SCRATCH LE:U32!
   s" adrp 2^20 pages on" T-LABEL
   [: $100000000 0 SCRATCH ARM64:CODE-ADDRESS! ;] E-A64EMIT-REACH TTHROWSQ
   s" adrp 2^20 - 1 pages on" T-LABEL
   $FFFFF000 0 SCRATCH ARM64:CODE-ADDRESS!  SCRATCH LE:U32@ $F07FFFE0 T=
   s" adrp across a page from its last word" T-LABEL
   $2000 $1FFC SCRATCH ARM64:CODE-ADDRESS!  SCRATCH $1FFC $2000 CHAIN-GIVES? TTRUE
   s" adrp back across a page from its first word" T-LABEL
   $1FFC $2000 SCRATCH ARM64:CODE-ADDRESS!  SCRATCH $2000 $1FFC CHAIN-GIVES? TTRUE ;


: CHECK ( -- )
   s" PL-RUN" ROW-NAMED {: r:n :}
   r 0< if s" arm64-place: PL-RUN has no shadow record" 1 die then
   r NSHADOW:EMISSION@ {: e:n :}
   r NSHADOW:RECORD@ XREF-REC {: rec:ptr :}
   s" a definition enters at its first byte" T-LABEL  r NSHADOW:ENTRY@ 0 T=
   e KINDS
   e rec XREF-START rec XREF-CODE-BYTES AT-HOST
   e rec XREF-START MOVE-BY + ELSEWHERE
   ENCODINGS
   REACH ;


public

: RUN ( -- )
   T-RESET
   TIER-CK
   NABI:BINDING NSHADOW:OPEN
   NSHADOW:RECORDS RECS0 !
   LOAD-FIXTURE
   CHECK
   NSHADOW:CLOSE
   T-REPORT ;

;package

ARM64-PLACE:RUN

;package

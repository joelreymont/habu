\ arm64-place.f - LINKER's arm64 link of a build's code. ARM64:ADOPT lays the
\ tree's stripped kernel object (HB-BUILD-CORE:KERNEL-BUILD) at the head of the
\ code buffer and ARM64:KERNEL-ENTRY maps the running hb's kernel primitive
\ entries to the same primitives' entries there.
\ ARM64:PLACE copies a fixture word's shadow emission: at the host routine's entry
\ it matches the routine T0 placed there, code address chains aside; elsewhere
\ its calls and chains reach what the shadow named. After REACH-ALL,
\ PLACE-ROUTINES lays every emission out after the kernel, calls reaching product
\ routines and kernel primitives at their link addresses, and a second placement
\ gives the same bytes. A link rooted at one word places only the emissions it
\ reaches, through calls or a code address; one rooted at PL-MAKE, whose body
\ calls `create`, is refused in a child process.
\ The fixture is test/compiler/arm64-place-fixture.f, loaded through the typed
\ interpret loop at tier 0 while a native shadow is open.

require src/lib/error.f
require src/lib/string.f
require src/lib/le.f
require src/lib/fs.f
require src/lib/test.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/lib/ffi-abi.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/interpret.f
require src/arch/arm64/asm.f
require src/habu/address-carrier.f
require src/arch/arm64/link.f
require src/arch/arm64/icode.f
require src/compiler/native/dict.f
require src/arch/arm64/link-kernel.f
require src/habu/link-band.f
require src/compiler/target/model.f
require tools/hb-build-core.f

package LINKER
private
package ARM64-PLACE

CAST: CODE>BYTES ( n -- ptr u8 )
CAST: N>U8 ( n -- ptr u8 )
CAST: XT>N ( xt -- n )
CAST: WORD>N ( [ -- ] -- n )

4096 constant SRC-CAP
SRC-CAP BUFFER: SRC                  \ the fixture's text
variable SRC-U
variable RECS0                       \ shadow records filed before the load
4096 constant OUT-CAP
OUT-CAP BUFFER: COPY                 \ a placed copy of the emission
16 BUFFER: SCRATCH                   \ one call site or code address chain

\ How a word of a placed copy may differ from the shadow's.
0 constant PLAIN
1 constant CALL-WORD
2 constant CODE-CHAIN
3 constant DATA-CHAIN

$123450 constant MOVE-BY             \ the second base's distance from the host's
\ What ELSEWHERE's resolvers and the band's data resolver add.
$10000 constant LINK-BY

variable IN-KERNEL
variable MAPPED
variable COMPARED
variable SAME
variable PCREL
variable KERNEL-END                  \ A64ICODE:ASM-LEN after ARM64:ADOPT
variable PRODUCT-CALLS
variable KERNEL-CALLS
DYNAMIC-BUFFER FIRST-BAND u8         \ the first placement's band


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

: REC-NAMED ( ptr u8 n -- n )
   ROW-NAMED {: r:n :}
   r 0< if s" arm64-place: a fixture word has no shadow record" 1 die then
   r ;


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


\ Every code address chain of the copy at dst, placed at at, gives the shadow's
\ value through map; the first site that does not, or -1.
: CHAINS-BAD ( n ptr u8 n [ n -- n ] -- n )
   {: e:n dst:ptr at:n map :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i CODE-CHAIN? if
         e i NSHADOW:ADDR-SITE@ {: s:n :}
         dst s +  at s +
         e s SHADOW-AT ADDRESS-CARRIER:ADDR-CHAIN-BYTES ADDRESS-CARRIER:CHAIN-VALUE
         map execute  CHAIN-GIVES? 0= if s unloop exit then
      then
   loop
   -1 ;


\ The first byte offset below n where a word of the copy outside its code address
\ chains differs from the host routine at h, or -1.
: HOST-DIFF ( n n n -- n )
   {: e:n h:n n:n :}
   n 4 / 0 ?do
      e i 4 * WORD-AT CODE-CHAIN <> if
         COPY i 4 * + LE:U32@  h i 4 * + CODE>BYTES LE:U32@ <> if
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


\ The first call row of the copy at dst, placed at at, that does not reach its
\ target through map, or -1.
: CALLS-BAD ( n ptr u8 n [ n -- n ] -- n )
   {: e:n dst:ptr at:n map :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      dst s +  at s +  e i NSHADOW:CALL-KIND@ CALL-TO
      e i NSHADOW:CALL-TARGET@ map execute <> if s unloop exit then
   loop
   -1 ;


\ The first data address row of the copy at dst whose chain does not hold the
\ shadow's value through map at the shadow's size, or -1.
: DATA-BAD ( n ptr u8 [ n -- n ] -- n )
   {: e:n dst:ptr map :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i DATA-ROW? if
         e i NSHADOW:ADDR-SITE@ {: s:n :}
         e i DATA-SIZE {: size:n :}
         dst s +  dst e NSHADOW:SIZE +  ADDRESS-CARRIER:CHAIN-SIZE size <>
         dst s + size ADDRESS-CARRIER:CHAIN-VALUE
         e s SHADOW-AT size ADDRESS-CARRIER:CHAIN-VALUE map execute <> or
         if s unloop exit then
      then
   loop
   -1 ;


\ The first plain word of the copy at dst that differs from the shadow's, or -1.
: PLAIN-DIFF ( n ptr u8 -- n )
   {: e:n dst:ptr :}
   e NSHADOW:SIZE 4 / 0 ?do
      e i 4 * WORD-AT PLAIN = if
         dst i 4 * + LE:U32@  e i 4 * SHADOW-AT LE:U32@ <> if
            i 4 * unloop exit
         then
      then
   loop
   -1 ;


\ Emission e copied to COPY to run at at, data addresses through data.
: PLACE-AT ( n n [ n -- n ] [ n -- n ] -- )
   {: e:n at:n data resolve :}
   e NSHADOW:SIZE OUT-CAP > if s" arm64-place: the emission outgrows COPY" 1 die then
   e COPY at data resolve ARM64:PLACE ;


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
   e h [: 0 + ;] [: 0 + ;] PLACE-AT
   s" placed at the host entry: same words" T-LABEL
   e h  hlen e NSHADOW:SIZE min  HOST-DIFF -1 T=
   s" placed at the host entry: code address chains" T-LABEL
   e COPY h [: 0 + ;] CHAINS-BAD -1 T= ;


\ At at with every target and data address moved by LINK-BY.
: ELSEWHERE ( n n -- )
   {: e:n at:n :}
   e at [: LINK-BY + ;] [: LINK-BY + ;] PLACE-AT
   s" placed elsewhere: calls" T-LABEL
   e COPY at [: LINK-BY + ;] CALLS-BAD -1 T=
   s" placed elsewhere: code address chains" T-LABEL
   e COPY at [: LINK-BY + ;] CHAINS-BAD -1 T=
   s" placed elsewhere: data addresses" T-LABEL
   e COPY [: LINK-BY + ;] DATA-BAD -1 T=
   s" placed elsewhere: other words" T-LABEL  e COPY PLAIN-DIFF -1 T= ;


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


\ b, bl, b.cond, cbz/cbnz, tbz/tbnz, adr/adrp and ldr (literal) encode a
\ pc-relative offset, so the same instruction can differ between the kernels.
: PC-RELATIVE? ( n -- bool )
   {: w:n :}
   w $7C000000 and $14000000 =
   w $FF000010 and $54000000 = or
   w $7E000000 and $34000000 = or
   w $7E000000 and $36000000 = or
   w $1F000000 and $10000000 = or
   w $3B000000 and $18000000 = or ;


: HOST-WORD ( n -- n ) N>U8 LE:U32@ ;
: PRODUCT-WORD ( n -- n ) A64ICODE:CODE swap + LE:U32@ ;


: COMPARE ( n -- )
   {: host:n :}
   host ARM64:KERNEL-ENTRY {: off:n :}
   off 0 < if exit then
   1 MAPPED +!
   host HOST-WORD {: w:n :}
   w PC-RELATIVE? if 1 PCREL +! exit then
   1 COMPARED +!
   off PRODUCT-WORD w = if 1 SAME +! then ;


\ The running hb's kernel primitives are the records that start in its own
\ code, [rbase, CODE-END-CELL).
: VISIT ( ptr n -- )
   {: rec:ptr :}
   rec XREF-START {: start:n :}
   start rbase < if exit then
   start data-base CODE-END-CELL + @ >= if exit then
   1 IN-KERNEL +!
   start COMPARE ;


: ENTRIES-CASE ( -- )
   s" every host kernel primitive maps to a product entry with its first word" T-LABEL
   0 IN-KERNEL !  0 MAPPED !  0 COMPARED !  0 SAME !  0 PCREL !
   ndict@ 0 ?do i XREF-REC VISIT loop
   IN-KERNEL @ 0 > TTRUE
   MAPPED @ IN-KERNEL @ T=
   SAME @ COMPARED @ T=
   s" kernel: primitives " type IN-KERNEL @ .
   s" compared " type COMPARED @ .
   s" skipped pc-relative " type PCREL @ . cr ;


: KERNEL-ENTRY-MAPS ( n -- ) ARM64:KERNEL-ENTRY 0 >= TTRUE ;
: MAPS ( xt -- ) XT>N KERNEL-ENTRY-MAPS ;


: NAMED-CASE ( -- )
   s" the primitives T0 compiles in by address map" T-LABEL
   ['] type MAPS
   ['] xt! MAPS
   ['] execute MAPS
   ['] die MAPS
   NDICT:DOES-PATCH-ENTRY KERNEL-ENTRY-MAPS ;


: THUNK-CASE ( -- )
   s" the callback thunk T0 bakes maps" T-LABEL
   -1 FFI:CALLBACK-ENTRY KERNEL-ENTRY-MAPS ;


: INTERIOR-CASE ( -- )
   s" an address inside a primitive maps to nothing" T-LABEL
   ['] dup XT>N 4 + ARM64:KERNEL-ENTRY -1 T= ;


: CHECK ( -- )
   s" PL-RUN" REC-NAMED {: r:n :}
   r NSHADOW:EMISSION@ {: e:n :}
   r NSHADOW:RECORD@ XREF-REC {: rec:ptr :}
   s" a definition enters at its first byte" T-LABEL  r NSHADOW:ENTRY@ 0 T=
   e KINDS
   e rec XREF-START rec XREF-CODE-BYTES AT-HOST
   e rec XREF-START MOVE-BY + ELSEWHERE
   ENCODINGS
   REACH ;


\ Emission e's placed copy in the band and the link address it runs at.
: BAND-AT ( n -- ptr u8 ) OFFSET@ A64ICODE:CODE swap + ;
: BAND-LINK ( n -- n ) OFFSET@ ARM64:TEXT-VA + ;

: BAND-CALLS ( n -- n ) {: e:n :} e e BAND-AT e BAND-LINK ['] LINK-ADDRESS CALLS-BAD ;
: BAND-CHAINS ( n -- n ) {: e:n :} e e BAND-AT e BAND-LINK ['] LINK-ADDRESS CHAINS-BAD ;
: BAND-DATA ( n -- n ) {: e:n :} e e BAND-AT [: LINK-BY + ;] DATA-BAD ;
: BAND-PLAIN ( n -- n ) {: e:n :} e e BAND-AT PLAIN-DIFF ;


\ The first emission whose placed copy check answers other than -1 for, or -1.
: FIRST-BAD ( [ n -- n ] -- n )
   {: check :}
   NSHADOW:EMISSIONS 0 ?do
      i check execute -1 <> if i unloop exit then
   loop
   -1 ;


\ A call to host address a counts as a product call when its link address is in
\ the band, as a kernel call when it is the kernel primitive's.
: COUNT-CALL ( n -- )
   {: a:n :}
   a LINK-ADDRESS {: to:n :}
   to ARM64:TEXT-VA KERNEL-END @ + >= if 1 PRODUCT-CALLS +! exit then
   a ARM64:KERNEL-ENTRY {: off:n :}
   off 0 >=  to ARM64:TEXT-VA off + =  and if 1 KERNEL-CALLS +! then ;


: COUNT-CALLS ( -- )
   0 PRODUCT-CALLS !  0 KERNEL-CALLS !
   NSHADOW:EMISSIONS 0 ?do
      i NSHADOW:CALL-SITES 0 ?do j i NSHADOW:CALL-TARGET@ COUNT-CALL loop
   loop ;


\ Whether call row k of emission e's placed copy decodes to to.
: CALL-REACHES? ( n n n -- bool )
   {: e:n k:n to:n :}
   e k NSHADOW:CALL-SITE@ {: s:n :}
   e BAND-AT s +  e BAND-LINK s +  e k NSHADOW:CALL-KIND@ CALL-TO  to = ;


: ANY-CALL-REACHES? ( n n -- bool )
   {: e:n to:n :}
   e NSHADOW:CALL-SITES 0 ?do
      e i to CALL-REACHES? if true unloop exit then
   loop
   false ;


\ The entry above 0 of a record over emission e: its does> clause, or -1.
: CLAUSE-ENTRY ( n -- n )
   {: e:n :}
   NSHADOW:RECORDS 0 ?do
      i NSHADOW:EMISSION@ e =  i NSHADOW:ENTRY@ 0 >  and if
         i NSHADOW:ENTRY@ unloop exit
      then
   loop
   -1 ;


: EMISSION-NAMED ( ptr u8 n -- n ) REC-NAMED NSHADOW:EMISSION@ ;
: HOST-NAMED ( ptr u8 n -- n ) REC-NAMED NSHADOW:RECORD@ XREF-REC XREF-START ;


: PLACED? ( ptr u8 n -- bool ) HOST-NAMED LINK-ADDRESS -1 <> ;


\ PL-FIVE's last call row is its does> slot (NSHADOW:PATCH-CALL?).
: SLOT-CASE ( ptr u8 n -- )
   T-LABEL
   s" PL-FIVE" EMISSION-NAMED {: five:n :}
   s" PL-MAKE" EMISSION-NAMED {: make:n :}
   make CLAUSE-ENTRY 0 > TTRUE
   five  five NSHADOW:CALL-SITES 1 -  make BAND-LINK make CLAUSE-ENTRY +
   CALL-REACHES? TTRUE ;


\ The band, cleared and placed again from the kernel's end, gives the bytes the
\ first placement did.
: SECOND-CASE ( -- )
   A64ICODE:ASM-LEN {: len:n :}
   len KERNEL-END @ - {: u:n :}
   A64ICODE:CODE KERNEL-END @ + {: band:ptr :}
   u FIRST-BAND-RESERVE
   band  0 FIRST-BAND  u BYTE-COPY
   u 0 ?do 0  band i +  c! loop
   KERNEL-END @ 4 / A64ICODE:ASM-CP !
   [: LINK-BY + ;] PLACE-ROUTINES
   s" band: a second placement gives the same bytes" T-LABEL
   A64ICODE:ASM-LEN len T=
   band u  0 FIRST-BAND u  STR= TTRUE
   FIRST-BAND-RELEASE ;


: BAND-CASE ( -- )
   ROUTINES REACH-ALL
   [: LINK-BY + ;] PLACE-ROUTINES
   s" band: every call" T-LABEL  ['] BAND-CALLS FIRST-BAD -1 T=
   s" band: every code address chain" T-LABEL  ['] BAND-CHAINS FIRST-BAD -1 T=
   s" band: every data address" T-LABEL  ['] BAND-DATA FIRST-BAD -1 T=
   s" band: every other word" T-LABEL  ['] BAND-PLAIN FIRST-BAD -1 T=
   COUNT-CALLS
   s" band: product and kernel calls" T-LABEL
   PRODUCT-CALLS @ 0 > TTRUE  KERNEL-CALLS @ 0 > TTRUE
   s" band: product calls " type PRODUCT-CALLS @ .
   s" kernel calls " type KERNEL-CALLS @ . cr
   s" PL-RUN" EMISSION-NAMED {: run:n :}
   s" PL-LEAF" EMISSION-NAMED {: leaf:n :}
   s" band: PL-RUN reaches PL-LEAF" T-LABEL
   run  leaf BAND-LINK  ANY-CALL-REACHES? TTRUE
   s" band: a word outside the product has no link address" T-LABEL
   ['] T-REPORT WORD>N LINK-ADDRESS -1 T=
   s" band: PL-FIVE's does> slot reaches PL-MAKE's clause" SLOT-CASE
   SECOND-CASE ;


\ A fresh link from the kernel's end that places what host address a reaches.
: LINK-FROM ( n -- )
   KERNEL-END @ 4 / A64ICODE:ASM-CP !
   ROUTINES
   ROOT
   [: LINK-BY + ;] PLACE-ROUTINES ;


\ PL-RUN's calls and code address chain reach PL-LEAF; its data address, PL-V,
\ is not followed.
: CLOSURE-CASE ( -- )
   s" PL-RUN" HOST-NAMED LINK-FROM
   s" PL-RUN" EMISSION-NAMED {: run:n :}
   s" PL-LEAF" EMISSION-NAMED {: leaf:n :}
   s" closure: PL-RUN and PL-LEAF are placed" T-LABEL
   s" PL-RUN" PLACED? TTRUE  s" PL-LEAF" PLACED? TTRUE
   s" closure: PL-MAKE, PL-FIVE and PL-V are not" T-LABEL
   s" PL-MAKE" PLACED? TFALSE  s" PL-FIVE" PLACED? TFALSE  s" PL-V" PLACED? TFALSE
   s" closure: the band holds PL-RUN and PL-LEAF" T-LABEL
   A64ICODE:ASM-LEN  KERNEL-END @ run NSHADOW:SIZE + leaf NSHADOW:SIZE +  T=
   s" closure: PL-RUN reaches PL-LEAF" T-LABEL
   run  leaf BAND-LINK  ANY-CALL-REACHES? TTRUE ;


\ PL-FIVE runs PL-MAKE's clause, never PL-MAKE's own body, which calls `create`:
\ the link places PL-MAKE's emission whole and is not refused.
: DOES-CASE ( -- )
   s" PL-FIVE" HOST-NAMED LINK-FROM
   s" does: PL-FIVE and PL-MAKE are placed" T-LABEL
   s" PL-FIVE" PLACED? TTRUE  s" PL-MAKE" PLACED? TTRUE
   s" does: PL-RUN, PL-LEAF, PL-V, PL-TICK and PL-TOCK are not" T-LABEL
   s" PL-RUN" PLACED? TFALSE  s" PL-LEAF" PLACED? TFALSE  s" PL-V" PLACED? TFALSE
   s" PL-TICK" PLACED? TFALSE  s" PL-TOCK" PLACED? TFALSE
   s" does: PL-MAKE's own body places PL-INIT" T-LABEL  s" PL-INIT" PLACED? TTRUE
   s" does: PL-FIVE's does> slot reaches PL-MAKE's clause" SLOT-CASE ;


: TICK-CASE ( -- )
   s" PL-TICK" HOST-NAMED LINK-FROM
   s" PL-TICK" EMISSION-NAMED {: tick:n :}
   s" PL-TOCK" EMISSION-NAMED {: tock:n :}
   s" tick: PL-TICK names PL-TOCK by a code address chain and calls it nowhere" T-LABEL
   tick CODE-CHAINS 0 > TTRUE  tick tock BAND-LINK ANY-CALL-REACHES? TFALSE
   s" tick: PL-TICK and PL-TOCK are placed" T-LABEL
   s" PL-TICK" PLACED? TTRUE  s" PL-TOCK" PLACED? TTRUE
   s" tick: PL-RUN, PL-LEAF, PL-MAKE and PL-INIT are not" T-LABEL
   s" PL-RUN" PLACED? TFALSE  s" PL-LEAF" PLACED? TFALSE
   s" PL-MAKE" PLACED? TFALSE  s" PL-INIT" PLACED? TFALSE
   s" tick: the band holds PL-TICK and PL-TOCK" T-LABEL
   A64ICODE:ASM-LEN  KERNEL-END @ tick NSHADOW:SIZE + tock NSHADOW:SIZE +  T=
   s" tick: PL-TICK's code address chain reaches PL-TOCK" T-LABEL
   tick BAND-CHAINS -1 T= ;


: UNSAFE-RUN ( CMD:command -- CMD:command )
   s" --load" CMD:ARG+  s" test/compiler/arm64-place-unsafe-bad.f" CMD:ARG+
   ENGINE-CANDIDATE:PATH$ 60000 >MS CMD:RUN-OUTCOME PROCESS:ENDING>RC RC>N {: rc:n :}
   s" refusal: the child arms before the link" T-LABEL
   CMD:OUT$ s\" ARM64-PLACE-UNSAFE-ARMED\n" T$=
   s" refusal: the child names the caller and the word" T-LABEL
   CMD:ERR$ s" link: PL-MAKE calls create, which a stripped program cannot run" CONTAINS? TTRUE
   s" refusal: the child's reason" T-LABEL
   CMD:ERR$ s" link: a stripped program reaches a word it cannot run" CONTAINS? TTRUE
   s" refusal: the child exits 74" T-LABEL  rc 74 T= ;


\ A link rooted at PL-MAKE, whose own body calls `create`, dies, so it runs in
\ a child process: test/compiler/arm64-place-unsafe-bad.f.
: REFUSAL-CASE ( -- ) [: UNSAFE-RUN ;] CMD:WITH-COMMAND ;


public

: RUN ( -- )
   T-RESET
   TIER-CK
   false RTARGET:HOST-TARGET HB-BUILD-CORE:KERNEL-BUILD
   ARM64:ADOPT
   A64ICODE:ASM-LEN KERNEL-END !
   ENTRIES-CASE
   NAMED-CASE
   THUNK-CASE
   INTERIOR-CASE
   \ PL-MAKE's `create` calls CREATEP-CELL's definer; the kernel's, there until
   \ this line, throws E-COMP-STATE in SHADOW-FIXED under a native shadow.
   OUTER:INSTALL-CREATE
   NABI:BINDING NSHADOW:OPEN-NATIVE
   NSHADOW:RECORDS RECS0 !
   LOAD-FIXTURE
   A64ICODE:ASM-LEN KERNEL-END @ <> if
      s" arm64-place: loading the fixture moved A64ICODE:ASM-LEN" 1 die
   then
   CHECK
   BAND-CASE
   CLOSURE-CASE
   DOES-CASE
   TICK-CASE
   NSHADOW:CLOSE
   REFUSAL-CASE
   T-REPORT ;

;package

ARM64-PLACE:RUN

;package

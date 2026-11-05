\ arm64-link-data.f - LINKER's link of a stripped build's DATA. Inside one
\ build, after ARM64:KERNEL, LINK-STRIPPED roots the closure at every xt cell's
\ value and appends the space's bytes and the DATA list after the routine band:
\ an xt cell's copy holds its routine's link address and is listed, a
\ quotation's included; a scratch cell's copy is 0 and a DATA pointer's DATA-VA
\ plus its target's offset, neither listed; a plain cell's is its bytes; a
\ placed data address site gives DATA-VA plus the variable's offset, and a
\ variable reached only by data addresses is not placed. A second link gives
\ the same bytes. One process: the boundary under test is in-process; only the
\ refusal runs in a child.
\ Fixture words are LD- and defined once per process: the host checker keeps a
\ symbol for each name a build defines.

require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/lib/test.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/address-carrier.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/habu/linker.f
require src/habu/link-band.f
require src/habu/link-data.f

package LINKER
private
package ARM64-LINK-DATA

CAST: CELL-XT ( n -- [ -- ptr n ] )

DYNAMIC-BUFFER FIRST-LINK u8         \ the first link's code buffer


\ LD-LEAF reaches LD-ADDR by a call and LD-LEAF2 only from the quotation LD-MAKE
\ stores; no code calls LD-LEAF or LD-MAKE.
: FIXTURE ( -- )
   s" variable LD-V  11 LD-V !" evaluate-closed
   s" variable LD-P  LD-V NULL-PTR - LD-P !  LD-P ptr-cell-mark" evaluate-closed
   s" : LD-ADDR ( -- ptr n ) LD-V ;" evaluate-closed
   s" : LD-LEAF ( n -- n ) LD-ADDR @ + ;" evaluate-closed
   s" : LD-LEAF2 ( n -- n ) 5 + ;" evaluate-closed
   s" variable LD-XT  ' LD-LEAF LD-XT xt!" evaluate-closed
   s" variable LD-SCR  7 LD-SCR !  LD-SCR scratch-cell-mark" evaluate-closed
   s" variable LD-Q" evaluate-closed
   s" : LD-MAKE ( -- ) [: LD-LEAF2 ;] LD-Q xt! ;" evaluate-closed
   s" LD-MAKE" evaluate-closed ;


: HOST-NAMED ( ptr u8 n -- n ) XREF-FIND-INDEX XREF-REC XREF-START ;

\ The shadow emission of the word named a u.
: EMISSION-NAMED ( ptr u8 n -- n )
   HOST-NAMED REC-AT {: r:n :}
   r 0< if s" arm64-link-data: a fixture word has no shadow record" 1 die then
   r NSHADOW:EMISSION@ ;

\ A fixture variable's offset in the space and the value it holds there.
: CELL-OFF ( ptr u8 n -- n ) HOST-NAMED CELL-XT execute NULL-PTR - BASE @ - ;
: SPACE-VALUE ( ptr u8 n -- n ) CELL-OFF SPACE-AT @ ;

\ The image copy of a fixture variable's cell.
: COPY-VALUE ( ptr u8 n -- n ) CELL-OFF COPY-AT LE:U64@ ;

: LISTED? ( n -- bool )
   {: off:n :}
   DATA-LIST-N @ 0 ?do
      A64ICODE:CODE DATA-LIST-AT @ + i 8 * + LE:U64@
      off = if true unloop exit then
   loop
   false ;

: LISTED-NAMED? ( ptr u8 n -- bool ) CELL-OFF LISTED? ;

: BAND-LINK ( n -- n ) OFFSET@ TEXT-VA + ;
: PLACED? ( ptr u8 n -- bool ) EMISSION-NAMED PLACED @ 0<> ;


: XT-CASE ( -- )
   s" LD-LEAF" EMISSION-NAMED {: leaf:n :}
   s" xt cell: its copy holds LD-LEAF's link address" T-LABEL
   s" LD-XT" COPY-VALUE  leaf BAND-LINK T=
   s" xt cell: it is in the DATA list" T-LABEL  s" LD-XT" LISTED-NAMED? TTRUE ;


\ The quotation's code lies in LD-MAKE's emission, so LD-MAKE is placed whole.
: QUOTE-CASE ( -- )
   s" LD-MAKE" EMISSION-NAMED {: make:n :}
   s" LD-Q" COPY-VALUE {: q:n :}
   s" quotation cell: its copy holds the quotation's link address" T-LABEL
   q make BAND-LINK >  q make BAND-LINK make NSHADOW:SIZE + <  and TTRUE
   s" quotation cell: it is in the DATA list" T-LABEL
   s" LD-Q" LISTED-NAMED? TTRUE ;


: DATA-CASE ( -- )
   s" scratch cell: its copy is 0" T-LABEL
   s" LD-SCR" SPACE-VALUE 7 T=  s" LD-SCR" COPY-VALUE 0 T=
   s" plain cell: its copy is its bytes" T-LABEL  s" LD-V" COPY-VALUE 11 T=
   s" scratch cell: not in the DATA list" T-LABEL
   s" LD-SCR" LISTED-NAMED? TFALSE
   s" DATA pointer: its copy is DATA-VA plus LD-V's offset" T-LABEL
   s" LD-P" COPY-VALUE  DATA-VA VA>N s" LD-V" CELL-OFF + T=
   s" DATA pointer: not in the DATA list" T-LABEL
   s" LD-P" LISTED-NAMED? TFALSE ;


\ The value of emission e's first data address chain in its placed copy, read
\ as test/compiler/arm64-place.f DATA-BAD reads it.
: DATA-CHAIN ( n -- n )
   {: e:n :}
   A64ICODE:CODE e OFFSET@ + {: band:ptr :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         band e i NSHADOW:ADDR-SITE@ + {: at:ptr :}
         at  band e NSHADOW:SIZE +  ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
         at size ADDRESS-CARRIER:CHAIN-VALUE unloop exit
      then
   loop
   -1 ;

: CHAIN-CASE ( -- )
   s" data address: LD-ADDR's chain is DATA-VA plus LD-V's offset" T-LABEL
   s" LD-ADDR" EMISSION-NAMED DATA-CHAIN  DATA-VA VA>N s" LD-V" CELL-OFF + T=
   s" data address: its variable LD-V is not placed" T-LABEL
   s" LD-V" PLACED? TFALSE ;


\ The kernel and a link again from an empty code buffer.
: SECOND-CASE ( -- )
   A64ICODE:ASM-LEN {: len:n :}
   len FIRST-LINK-RESERVE
   A64ICODE:CODE  0 FIRST-LINK  len BYTE-COPY
   ARM64:KERNEL
   LINK-STRIPPED
   s" a second link gives the same code buffer bytes" T-LABEL
   A64ICODE:ASM-LEN len T=
   A64ICODE:CODE len  0 FIRST-LINK len  STR= TTRUE
   FIRST-LINK-RELEASE ;


: BODY ( -- )
   FIXTURE
   ARM64:KERNEL
   LINK-STRIPPED
   XT-CASE QUOTE-CASE DATA-CASE CHAIN-CASE
   SECOND-CASE ;


: BAD-RUN ( CMD:command -- CMD:command )
   s" --load" CMD:ARG+  s" test/compiler/arm64-link-data-bad.f" CMD:ARG+
   ENGINE-CANDIDATE:PATH$ 60000 >MS CMD:RUN-OUTCOME PROC-OUTCOME>RC RC>N {: rc:n :}
   s" refusal: the child names the value" T-LABEL
   CMD:OUT$
   s\" ARM64-LINK-DATA-BAD-ARMED\nlink: a DATA pointer outside the build's space: 1\n"
   T$=
   s" refusal: the child's reason" T-LABEL
   CMD:ERR$ s" link: a DATA pointer outside the build's space" CONTAINS? TTRUE
   s" refusal: the child exits 74" T-LABEL  rc 74 T= ;

\ The refusal dies, so it runs in a child: test/compiler/arm64-link-data-bad.f.
: REFUSAL-CASE ( -- ) [: BAD-RUN ;] CMD:WITH-COMMAND ;


public

: RUN ( -- )
   T-RESET
   s" app" [: BODY ;] LINKER:WITH
   REFUSAL-CASE
   T-REPORT ;

;package

ARM64-LINK-DATA:RUN

;package

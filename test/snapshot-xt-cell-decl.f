\ snapshot-xt-cell-decl.f - a snapshot keeps DATA byte for byte but boots with a
\ fresh JIT region, so persisted xt cells are relocated from a table filled
\ where each cell's kind is declared (`defer`, `is`, cold boot's hook cells),
\ never by reading contents (src/habu/layout.f SNAP-RELOC:XTCELL-*). An ordinary
\ cell holding a region-shaped value or a live token must stay out of the table.

require src/habu/address-cells.f
require src/lib/errors.f
require src/lib/test.f

package XT-CELL-DECL
private

variable N0   variable N1   variable N3
PTR-VARIABLE FORGED-A
variable RAW-XT
variable HIT

: COUNT@ ( -- n )
   data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: ROW@ ( n -- n ) {: row:n :}
   row ADDRESS-CELLS:ROW@ ;

: LISTED? ( n -- bool ) {: off:n :}
   0 HIT !
   COUNT@ 0 ?do
      i ROW@ off = if 1 HIT ! then
   loop
   HIT @ 0 <> ;

\ FORGED-A is a declared pointer cell naming the forged cell, the anonymous
\ `0 ,` allotment that nothing declares.
: FORGE ( -- )
   here FORGED-A !
   0 ,
   dbase@ 16 +  FORGED-A @ ! ;

: FORGED-OFF ( -- n )
   FORGED-A @ data-base - ;

: RAW-OFF ( -- n )
   RAW-XT data-base - ;

: TAKE0 ( -- ) COUNT@ N0 ! ;
: TAKE1 ( -- ) COUNT@ N1 ! ;
: TAKE3 ( -- ) COUNT@ N3 ! ;

TAKE0

FORGE

TAKE1

: RAW-TARGET ( -- n ) 99 ;
CAST: XT>N ( [ -- n ] -- n )
: RAW-TARGET-XT ( -- n ) ['] RAW-TARGET XT>N ;

\ Checked source refuses storing the token itself (E-RAW-CELL-PTR) and `xt!`
\ would declare the cell, so the integer XT>N projects is stored.
: RAW-ARM ( -- ) RAW-TARGET-XT RAW-XT ! ;

RAW-ARM

TAKE3

public

: RUN ( -- )
   T-RESET
   s" storing a region-shaped value in an ordinary cell declares nothing" T-LABEL
   N1 @ N0 @ T=
   s" the forged cell is not in the relocation table" T-LABEL
   FORGED-OFF LISTED? 0= TTRUE
   s" the forged cell keeps the exact value that was stored" T-LABEL
   FORGED-A @ @  dbase@ 16 +  T=
   s" storing a real execution token in an ordinary cell declares nothing" T-LABEL
   N3 @ N1 @ T=
   s" the ordinary cell holding a real token is not in the table" T-LABEL
   RAW-OFF LISTED? 0= TTRUE
   s" that cell really does hold the token that was stored" T-LABEL
   RAW-XT @  RAW-TARGET-XT  T=
   T-REPORT
   s" snapshot-xt-cell-decl-test: ok" type cr ;

;package

XT-CELL-DECL:RUN

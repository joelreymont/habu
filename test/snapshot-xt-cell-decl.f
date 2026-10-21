\ snapshot-xt-cell-decl.f - the declared-kind rule for persisted cells that hold a
\ JIT-region address (dot habu-relocate-persisted-defer-7aa681c4).
\
\ A snapshot image keeps the DATA region byte for byte but gets a fresh JIT region
\ from the kernel on every boot, so any persisted cell that holds an execution
\ token has to be moved with the region when the image is restored. The engine
\ finds those cells from a table it fills in where each cell's kind is DECIDED --
\ `defer` when it allocates a dispatch cell, `is` when it stores an execution
\ token into one, and cold boot for the fixed engine hook/dispatch cells -- and never by
\ looking at what a cell contains. This suite pins that rule against the live
\ engine by watching the table itself (src/habu/layout.f SNAP-RELOC:XTCELL-*).
\
\ The negative case is the point of the design: an ordinary heap cell is given a
\ value that is indistinguishable from a live execution token -- an address inside
\ the running JIT region -- and must still be left alone, because nothing declared
\ it. A relocation pass that decided membership by looking for values in the
\ region's address range would relocate that cell and corrupt an ordinary integer.

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

\ Is this DATA offset one of the declared address cells?
: LISTED? ( n -- bool ) {: off:n :}
   0 HIT !
   COUNT@ 0 ?do
      i ROW@ off = if 1 HIT ! then
   loop
   HIT @ 0 <> ;

\ An ordinary heap cell holding a value that looks exactly like an execution
\ token: an address a little way into the live JIT region. Nothing about the store
\ declares anything, so the cell must stay out of the table. FORGED-A is only the
\ HANDLE that names it and is a declared pointer cell like any other address-
\ carrying slot; the forged cell is the anonymous `0 ,` allotment, and nothing
\ declares that.
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

\ The shape the lowering-certificate producers used to have (dot
\ habu-declare-persisted-producer-76fbce09), and the reason a restored image jumped
\ into nothing on its first checked definition: an ordinary `variable` holding a real
\ execution token, put there by an ordinary `!`. Its contents are not merely
\ region-SHAPED like the forged cell above — they are an actual live token, the
\ same kind of value a declared cell holds — and it still must stay out of the
\ table, because nothing about a `variable` and a store decides a cell's kind.

: RAW-TARGET ( -- n ) 99 ;
\ A target's code address as a cell value.
CAST: XT>N ( [ -- n ] -- n )
: RAW-TARGET-XT ( -- n ) ['] RAW-TARGET XT>N ;

\ The store is the point: an ORDINARY `!` of a live token's bits into an
\ undeclared cell. Checked source may not store the token itself
\ (E-RAW-CELL-PTR, "an undeclared cell cannot hold an execution token"), and
\ `xt!` would declare the cell -- the opposite of what this subject measures --
\ so the token is stored as the integer XT>N projects; the cell holds the same
\ live token either way.
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

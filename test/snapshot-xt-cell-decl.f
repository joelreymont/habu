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
\ engine, through the real `defer` and `is` handlers, by watching the table itself
\ (src/habu/layout.f SNAP-RELOC:XTCELL-*).
\
\ The negative case is the point of the design: an ordinary heap cell is given a
\ value that is indistinguishable from a live execution token -- an address inside
\ the running JIT region -- and must still be left alone, because nothing declared
\ it. A relocation pass that decided membership by looking for values in the
\ region's address range would relocate that cell, corrupt an ordinary integer,
\ and pass this file's positive cases while failing here.

require src/habu/address-cells.f
require src/lib/errors.f
require src/lib/test.f

package XT-CELL-DECL
private

variable N0   variable N1   variable N2   variable N3   variable N4
variable N5   variable N6   variable N7   variable N8   variable N9
variable P0   variable P1   variable P2
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

\ A declared cell's DATA offset. `-` over a pointer to a pointer and the DATA
\ base would make the base the address of an address, which the base fence
\ refuses at every pointee depth; the distance is measured in bytes through the
\ same BYTE-VIEW door src/habu/snap-lib.f SND-ZERO-OFF takes.
: DATA-OFF ( ptr a -- n )
   BYTE-VIEW data-base BYTE-VIEW - ;

: PTR-LISTED? ( n -- bool ) {: off:n :}
   0 HIT !
   COUNT@ 0 ?do
      i ROW@ {: row:n :}
      row SNAP-RELOC:XTCELL-DATA-TAG and 0 <>
      row SNAP-RELOC:XTCELL-OFF-MASK and off = and if 1 HIT ! then
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
: TAKE2 ( -- ) COUNT@ N2 ! ;
: TAKE3 ( -- ) COUNT@ N3 ! ;
: TAKE4 ( -- ) COUNT@ N4 ! ;
: TAKE5 ( -- ) COUNT@ N5 ! ;
: TAKE6 ( -- ) COUNT@ N6 ! ;
: TAKE7 ( -- ) COUNT@ N7 ! ;
: TAKE8 ( -- ) COUNT@ N8 ! ;
: TAKE9 ( -- ) COUNT@ N9 ! ;

COUNT@ P0 !
PTR-VARIABLE SCRATCH-PTR-SLOT
COUNT@ P1 !
PERSISTED-PTR-VARIABLE PTR-SLOT
COUNT@ P2 !

TAKE0

defer PROBE-D ( -- n )

TAKE1

\ `is` runs while the installer word is being compiled, so the declaration is made
\ here; the store into the cell happens later, when the installer is executed.
: INSTALL-FIRST ( -- ) [: 4242 ;] is PROBE-D ;

TAKE2

: INSTALL-SECOND ( -- ) [: 7 ;] is PROBE-D ;

TAKE3

INSTALL-FIRST
INSTALL-SECOND

FORGE

TAKE4

\ The shape the two lowering-certificate producers now use (dot
\ habu-declare-persisted-producer-76fbce09): a package-private `defer` plus a
\ PUBLIC installer that takes the producer as a QUOTATION and stores it with
\ `is`. The declaration is made where the installer's `is` is COMPILED, so one
\ installer serves every later install and the cell is declared exactly once
\ however many producers are handed to it.

TAKE5

defer PROBE-Q ( -- n )

TAKE6

: Q-INSTALL ( [ -- n ] -- ) is PROBE-Q ;

: ARM-FIRST ( -- ) [: 99 ;] Q-INSTALL ;
: ARM-SECOND ( -- ) [: 5 ;] Q-INSTALL ;

TAKE7

ARM-FIRST
ARM-SECOND

TAKE8

\ The shape they USED to have, and the reason a restored image jumped into
\ nothing on its first checked definition: an ordinary `variable` holding a real
\ execution token, put there by an ordinary `!`. Its contents are not merely
\ region-SHAPED like the forged cell above — they are an actual live token, the
\ same kind of value the declared cell holds — and it still must stay out of the
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

TAKE9

public

: RUN ( -- )
   T-RESET
   s" ordinary PTR-VARIABLE declares no relocation row" T-LABEL
   P1 @ P0 @ T=
   s" PERSISTED-PTR-VARIABLE declares exactly one DATA-pointer cell" T-LABEL
   P2 @ P1 @ 1+ T=
   s" that cell is tagged as a DATA pointer in the shared table" T-LABEL
   PTR-SLOT DATA-OFF PTR-LISTED? TTRUE
   s" the structural declaration does not change its initial null value" T-LABEL
   PTR-SLOT @ 0= TTRUE
   s" defer declares exactly one new address cell" T-LABEL
   N1 @ N0 @ 1+ T=
   s" is on an already declared cell adds no second row" T-LABEL
   N2 @ N1 @ T=
   s" a second is still adds no row" T-LABEL
   N3 @ N2 @ T=
   s" storing a region-shaped value in an ordinary cell declares nothing" T-LABEL
   N4 @ N3 @ T=
   s" the forged cell is not in the relocation table" T-LABEL
   FORGED-OFF LISTED? 0= TTRUE
   s" the forged cell keeps the exact value that was stored" T-LABEL
   FORGED-A @ @  dbase@ 16 +  T=
   s" the deferred word dispatches through the declared cell" T-LABEL
   PROBE-D 7 T=
   s" a defer behind a quotation installer declares exactly one cell" T-LABEL
   N6 @ N5 @ 1+ T=
   s" compiling the installer re-declares that cell and adds no row" T-LABEL
   N7 @ N6 @ T=
   s" running the installer twice adds no row" T-LABEL
   N8 @ N7 @ T=
   s" the installed producer is the one the last install handed over" T-LABEL
   PROBE-Q 5 T=
   s" storing a real execution token in an ordinary cell declares nothing" T-LABEL
   N9 @ N8 @ T=
   s" the ordinary cell holding a real token is not in the table" T-LABEL
   RAW-OFF LISTED? 0= TTRUE
   s" that cell really does hold the token that was stored" T-LABEL
   RAW-XT @  RAW-TARGET-XT  T=
   T-REPORT
   s" snapshot-xt-cell-decl-test: ok" type cr ;

;package

XT-CELL-DECL:RUN

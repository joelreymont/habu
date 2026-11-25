\ `create … allot` reads zero after a negative allot, which lowers the data
\ pointer over written bytes.
require src/lib/test/assert.f

package DICTIONARY-ZERO-TEST

\ A negative allot raises the release high-water, below which allot zeroes the
\ space it reuses; fresh DATA pages are zero.
: ZERO3? ( ^n -- bool )
   {: p:^ :}
   p @ 0=  p cell+ @ 0= and  p 2 cells + @ 0= and ;

\ Each released block is written whole and is a cell larger than the block
\ allotted over it, which lies inside it wherever the declaration's gap ends.
create NEG-A 4 cells allot
-1 NEG-A !  -1 NEG-A cell+ !  -1 NEG-A 2 cells + !  -1 NEG-A 3 cells + !
-4 cells allot
create NEG-B 3 cells allot

\ The ARM64 engine's `variable` takes its cell without `allot`, so it is a
\ case of its own.
create NEGV-A 2 cells allot
-1 NEGV-A !  -1 NEGV-A cell+ !
-2 cells allot
variable NEGV-B

: RUN ( -- )
   T-RESET
   s" a negative allot releases zeroed space" T-LABEL
   NEG-B NEG-A - dup 0 >= swap 1 cells <= and TTRUE
   NEG-B ZERO3? TTRUE
   s" a variable after a negative allot reads zero" T-LABEL
   NEGV-B NEGV-A - dup 0 >= swap 1 cells <= and TTRUE
   NEGV-B @ 0 T=
   T-REPORT ;
RUN
;package

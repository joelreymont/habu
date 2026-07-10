\ layout-box-test.f - focused tests for the boxed-layout record arena.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/layout/box.f

2 constant BOXT-M               \ a 2-payload-cell test variant
7 constant BOXT-TAG
111 constant BOXT-P0
222 constant BOXT-P1

\ a kept box pointer (ptr-field idiom, so it survives across later allocations)
variable BOXT-KEEP
: BOXT-KEEP-FIELD ( -- ptr ptr a )   BOXT-KEEP 0 ptr-field ;
: BOXT-KEEP@ ( -- ptr a )   BOXT-KEEP-FIELD @ ;
: BOXT-KEEP! ( ptr a -- )   BOXT-KEEP-FIELD ! ;

: BOXT-ZERO-INIT ( -- )         \ a fresh record reads tag 0 and every payload 0
   BOXT-M BOX-ALLOC {: p:ptr :}
   p BOX-DEREF-TAG 0 T=
   0 p BOX-PAY@ 0 T=
   1 p BOX-PAY@ 0 T= ;

: BOXT-ROUNDTRIP ( -- )         \ write the tag + payloads, read them back
   BOXT-M BOX-ALLOC {: p:ptr :}
   BOXT-TAG p BOX-TAG!
   BOXT-P0 0 p BOX-PAY!
   BOXT-P1 1 p BOX-PAY!
   p BOX-DEREF-TAG BOXT-TAG T=
   0 p BOX-PAY@ BOXT-P0 T=
   1 p BOX-PAY@ BOXT-P1 T= ;

: BOXT-DISTINCT ( -- )          \ two boxes have distinct, independent storage: if they
   BOXT-M BOX-ALLOC {: a:ptr :} \ aliased, the second write below would clobber the first's tag
   BOXT-M BOX-ALLOC {: b:ptr :}
   1 a BOX-TAG!
   2 b BOX-TAG!
   a BOX-DEREF-TAG 1 T=
   b BOX-DEREF-TAG 2 T= ;

: BOXT-FILL ( n -- )            \ allocate n throwaway 1-payload records (drives chunk growth)
   {: cnt:n :}
   0 cnt ?do  1 BOX-ALLOC drop  loop ;

: BOXT-GROWTH ( -- )            \ a record survives a later chunk-boundary crossing
   BOXT-M BOX-ALLOC BOXT-KEEP!
   BOXT-TAG BOXT-KEEP@ BOX-TAG!
   BOXT-P0 0 BOXT-KEEP@ BOX-PAY!
   BOX-CHUNK-CELLS BOXT-FILL     \ ~2 chunks of records -> forces a grow
   BOXT-KEEP@ BOX-DEREF-TAG BOXT-TAG T=
   0 BOXT-KEEP@ BOX-PAY@ BOXT-P0 T= ;

: BOXT-RESET ( -- )             \ after RESET, the next box comes from a fresh zeroed chunk
   BOXT-M BOX-ALLOC {: p:ptr :}
   99 p BOX-TAG!
   BOX-ARENA-RESET
   BOXT-M BOX-ALLOC {: q:ptr :}
   q BOX-DEREF-TAG 0 T= ;

T-RESET
BOX-CHUNK-CELLS 8192 T=          \ 64K page of cells
0 BOX-RECORD-CELLS 1 T=          \ tag only
1 BOX-RECORD-CELLS 2 T=
3 BOX-RECORD-CELLS 4 T=
BOXT-ZERO-INIT
BOXT-ROUNDTRIP
BOXT-DISTINCT
BOXT-GROWTH
BOXT-RESET
T-REPORT
s" layout-box-test: ok" type cr

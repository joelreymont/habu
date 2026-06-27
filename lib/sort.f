\ sort.f - in-place, fully checked heapsort of cell arrays with a comparator.
\
\ SORT! ( ptr a n [ a a -- bool ] -- ) sorts any cell array using a "less-than"
\ comparator quotation (true if the first element should precede the second):
\ floats with [: f< ;], ints with [: < ;], descending with the reversed test.
\ Binary heapsort: O(n log n), in place, no scratch buffer. FSORT! is the
\ float-ascending convenience. The comparator is threaded as an ordinary checked
\ higher-order parameter — the checker verifies quotation effects through the
\ call chain and the sift-down loop, so no unchecked boundary is needed (unlike
\ the older src/core/combinators.f). Raw indexed access (array.f's A-SUM idiom);
\ the algorithm keeps indices in bounds; len <= 1 is a no-op. Core + float deps.

variable HS-NODE                           \ sift-down cursor
variable HS-I                              \ build / extract loop index

: FX@ ( ptr a n -- a ) {: a:ptr idx :}  a idx cells + @ ;
: FX-SWAP ( ptr a n n -- ) {: a:ptr ix jx :}
   a ix FX@  a jx FX@  {: vi vj :}
   vj a ix cells + !   vi a jx cells + ! ;

\ Index of the heap-larger child of HS-NODE within heap[0..size), or -1 if a leaf.
: HS-CHILD ( ptr a n [ a a -- bool ] -- n ) {: a:ptr size q :}
   HS-NODE @ 2 * 1 + {: left :}
   left 1 + {: right :}
   left size >= if -1 else
      right size >= if left else
         a left FX@  a right FX@  q execute if right else left then
      then
   then ;

\ One sift-down step: if HS-NODE precedes its larger child, swap down + continue.
: HS-STEP ( ptr a n [ a a -- bool ] -- bool ) {: a:ptr size q :}
   a size q HS-CHILD {: c :}
   c 0 < if 0 0= 0= exit then
   a HS-NODE @ FX@  a c FX@  q execute if
      a HS-NODE @ c FX-SWAP  c HS-NODE !  0 0=
   else 0 0= 0= then ;

: HS-SIFT ( ptr a n n [ a a -- bool ] -- ) {: a:ptr size root q :}
   root HS-NODE !
   begin a size q HS-STEP while repeat ;

: SORT! ( ptr a n [ a a -- bool ] -- ) {: a:ptr len q :}
   len 2 / 1 - HS-I !                       \ build the heap from the last parent down
   begin HS-I @ 0 >= while  a len HS-I @ q HS-SIFT  HS-I @ 1 - HS-I !  repeat
   len 1 - HS-I !                           \ move the extreme to the tail, shrink, re-sift
   begin HS-I @ 1 >= while  a 0 HS-I @ FX-SWAP  a HS-I @ 0 q HS-SIFT  HS-I @ 1 - HS-I !  repeat ;

\ Float-ascending convenience (the percentile/median path in lib/stats.f).
: FSORT! ( ptr r n -- )  [: f< ;] SORT! ;

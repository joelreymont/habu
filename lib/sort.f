\ sort.f - in-place, fully checked heapsort of cell arrays with a comparator.
\
\ SORT:SORT! ( ptr a n [ a a -- bool ] -- ) sorts any cell array using a "less-than"
\ comparator quotation (true if the first element should precede the second):
\ floats with [: f< ;], ints with [: < ;], descending with the reversed test.
\ Binary heapsort: O(n log n), in place, no scratch buffer. SORT:FSORT! is the
\ float-ascending convenience. The comparator is threaded as an ordinary checked
\ higher-order parameter — the checker verifies quotation effects through the
\ call chain and the sift-down loop, so no unchecked boundary is needed (unlike
\ the older src/core/combinators.f). Raw indexed access (array.f's A-SUM idiom);
\ the algorithm keeps indices in bounds; len <= 1 is a no-op. Core + float deps.
\
\ The module lives in `package SORT`. External callers use the qualified public
\ API (SORT:SORT!, SORT:FSORT!, and SORT:FX@ for the float-cell read that
\ lib/stats.f shares); the heap cursor and sift-down helpers are package-private.

package SORT

public

: FX@ ( ptr a n -- a ) {: a:ptr idx :}  a idx cells + @ ;

private
: FX-SWAP ( ptr a n n -- ) {: a:ptr ix jx :}
   a ix FX@  a jx FX@  {: vi vj :}
   vj a ix cells + !   vi a jx cells + ! ;

\ Index of the heap-larger child of node within heap[0..size), or -1 if a leaf.
: HS-CHILD ( ptr a n n [ a a -- bool ] -- n ) {: a:ptr size node q :}
   node 2 * 1 + {: left :}
   left 1 + {: right :}
   left size >= if -1 else
      right size >= if left else
         a left FX@  a right FX@  q execute if right else left then
      then
   then ;

\ Keep each cursor on its invocation's stack: comparators may themselves sort.
: HS-STEP ( ptr a n n [ a a -- bool ] -- n bool ) {: a:ptr size node q :}
   a size node q HS-CHILD {: c :}
   c 0 < if node false exit then
   a node FX@  a c FX@  q execute if
      a node c FX-SWAP  c true
   else node false then ;

: HS-SIFT ( ptr a n n [ a a -- bool ] -- ) {: a:ptr size root q :}
   root begin {: node :} a size node q HS-STEP while repeat drop ;

public

: SORT! ( ptr a n [ a a -- bool ] -- ) {: a:ptr len q :}
   len 1 <= if exit then
   len 2 / 1 -                             \ build the heap from the last parent down
   begin dup 0 >= while {: root :} a len root q HS-SIFT root 1 - repeat drop
   len 1 -                                 \ move the extreme to the tail, shrink, re-sift
   begin dup 1 >= while {: tail :}
      a 0 tail FX-SWAP a tail 0 q HS-SIFT tail 1 -
   repeat drop ;

\ Float-ascending convenience (the percentile/median path in lib/stats.f).
: FSORT! ( ptr r n -- )  [: f< ;] SORT! ;

;package

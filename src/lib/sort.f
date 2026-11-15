\ sort.f - package SORT: in-place checked heapsort of cell arrays, O(n log n)
\ with no scratch buffer, and binary search. SORT! takes a less-than comparator
\ ([: f< ;] for floats, [: < ;] for ints, the reversed test for descending).
\ Raw indexed access stays in bounds by construction.

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
   len 2 / 1 -
   begin dup 0 >= while {: root :} a len root q HS-SIFT root 1 - repeat drop
   len 1 -
   begin dup 1 >= while {: tail :}
      a 0 tail FX-SWAP a tail 0 q HS-SIFT tail 1 -
   repeat drop ;

: FSORT! ( ptr r n -- )  [: f< ;] SORT! ;

\ The first index of [lo, hi) where q answers false, hi when none does: q answers
\ true on a leading run of the range and false after it.
: PARTITION-POINT ( n n [ n -- bool ] -- n )
   {: q :}
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid q execute if mid 1 + hi else lo mid then
   repeat
   drop ;

;package

\ sort.f - in-place ascending sort of float cell arrays.
\
\ Binary heapsort: O(n log n), in place, no scratch buffer, no recursion (the
\ sift-down is a loop). Compares with `f<`, so the array is `ptr r len` (floats
\ stored one per cell). Uses raw indexed cell access (the same idiom array.f's own
\ A-SUM/A-MIN loops use); bounds are guaranteed by the algorithm. len <= 1 (and
\ any negative len) sorts to a no-op. Depends only on core + float ops.

variable HS-NODE                           \ sift-down cursor
variable HS-I                              \ build / extract loop index

: FX@ ( ptr r n -- r ) {: a:ptr idx :}  a idx cells + @ ;
: FX-SWAP ( ptr r n n -- ) {: a:ptr ix jx :}
   a ix FX@  a jx FX@  {: vi vj :}
   vj a ix cells + !   vi a jx cells + ! ;

\ Index of the larger child of HS-NODE within heap[0..size), or -1 if it is a
\ leaf. Both child indices are computed before the bounds branches so no local is
\ bound inside control flow.
: HS-CHILD ( ptr r n -- n ) {: a:ptr size :}
   HS-NODE @ 2 * 1 + {: left :}
   left 1 + {: right :}
   left size >= if -1 else
      right size >= if left else
         a left FX@  a right FX@  f< if right else left then
      then
   then ;

\ One sift-down step: if HS-NODE is smaller than its larger child, swap down and
\ report continue; else report stop. Maintains a max-heap.
: HS-STEP ( ptr r n -- bool ) {: a:ptr size :}
   a size HS-CHILD {: c :}
   c 0 < if 0 0= 0= exit then
   a HS-NODE @ FX@   a c FX@   f< if
      a HS-NODE @ c FX-SWAP
      c HS-NODE !  0 0=
   else 0 0= 0= then ;

: HS-SIFT ( ptr r n n -- ) {: a:ptr size root :}    \ sift root down through heap[0..size)
   root HS-NODE !
   begin a size HS-STEP while repeat ;

\ Sort len float cells ascending.
: FSORT! ( ptr r n -- ) {: a:ptr len :}
   len 2 / 1 - HS-I !                       \ build max-heap from the last parent down
   begin HS-I @ 0 >= while
      a len HS-I @ HS-SIFT
      HS-I @ 1 - HS-I !
   repeat
   len 1 - HS-I !                           \ repeatedly move the max to the tail
   begin HS-I @ 1 >= while
      a 0 HS-I @ FX-SWAP
      a HS-I @ 0 HS-SIFT
      HS-I @ 1 - HS-I !
   repeat ;

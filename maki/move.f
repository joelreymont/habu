\ maki/move.f - buffer-level movement reference execution (the golden oracles).
\
\ Movement ops (reshape/transpose/slice/concat/gather; CAD-PLAN 4.2
\ CLASS-MOVEMENT) carry NO compute - they are exact layout rewrites. These host
\ references define the v1 semantics over the 2D row-major model: the golden
\ oracle the planner's dissolution must reproduce whenever it DOES materialize a
\ movement node. Buffer-level in the maki/array.f style: contiguous float-cell
\ buffers (base ptr) addressed row-major (row cols * + col), reusing T-GET/T-SET.
\ Exact (NUM-EXACT): a straight element copy, never an approximation. Fail closed
\ on mismatched extents (reshape/transpose/concat) or out-of-range slice ranges /
\ gather indices. maki -> habu only; move owns -5065..-5067.

require maki/array.f

-5065 constant E-MV-EXTENT   \ element / column counts disagree (reshape/transpose/concat)
-5066 constant E-MV-RANGE    \ slice row range outside [0,rows] or inverted (r0 > r1)
-5067 constant E-MV-INDEX    \ gather index outside [0,rows)

package MAKI
private

\ copy n contiguous float cells  src[0..n) -> dst[0..n)
: MV-COPY ( ptr a ptr a n -- ) {: s:ptr d:ptr n:n :}
   n 0 ?do  s i T-GET  d i T-SET  loop ;

\ copy one cols-wide row: src row sr -> dst row dr (both cols wide, row-major)
: MV-ROW ( ptr a n ptr a n n -- ) {: s:ptr sr:n d:ptr dr:n cols:n :}
   cols 0 ?do  s sr cols * i +  T-GET   d dr cols * i +  T-SET  loop ;

public

\ reshape: same elements, new shape. Row-major order is preserved, so once the
\ element counts agree it is a straight contiguous copy.
: MOVE-RESHAPE ( ptr a n n ptr a n n -- ) {: s:ptr sr:n sc:n d:ptr dr:n dc:n :}
   sr sc *  dr dc *  <> if E-MV-EXTENT throw then
   s d  sr sc *  MV-COPY ;

\ transpose: dst[k,i] = src[i,k]; dst must be sc x sr (dr=sc, dc=sr).
: MOVE-TRANSPOSE ( ptr a n n ptr a n n -- ) {: s:ptr sr:n sc:n d:ptr dr:n dc:n :}
   dr sc <> dc sr <> or if E-MV-EXTENT throw then
   sr 0 ?do                                      \ j = outer = src row i
      sc 0 ?do                                   \ i = inner = src col k
         s  j sc * i +  T-GET                     \ src[j,i]
         d  i sr * j +  T-SET                     \ dst[i,j] (dst is sc x sr)
      loop
   loop ;

\ slice: copy rows [r0,r1) of a sr x sc buffer into dst ((r1-r0) x sc).
: MOVE-SLICE ( ptr a n n n n ptr a -- ) {: s:ptr sr:n sc:n r0:n r1:n d:ptr :}
   r0 0 < r1 sr > or  r0 r1 > or if E-MV-RANGE throw then
   r1 r0 - 0 ?do  s  r0 i +  d i  sc  MV-ROW  loop ;

\ concat: row-append A (ar x ac) then B (br x bc); requires ac=bc; dst (ar+br) x ac.
: MOVE-CONCAT ( ptr a n n ptr a n n ptr a -- ) {: a:ptr ar:n ac:n b:ptr br:n bc:n d:ptr :}
   ac bc <> if E-MV-EXTENT throw then
   ar 0 ?do  a i     d i       ac MV-ROW  loop
   br 0 ?do  b i     d ar i +  ac MV-ROW  loop ;

\ gather: dst row k = src row idx[k]; idx is `in` integer cells; dst (in x sc).
: MOVE-GATHER ( ptr a n n ptr a n ptr a -- ) {: s:ptr sr:n sc:n ix:ptr in:n d:ptr :}
   in 0 ?do
      ix i cells + @  dup 0 < over sr >= or if E-MV-INDEX throw then
      s swap  d i  sc  MV-ROW                     \ ( sbase idx dbase i sc -- )
   loop ;

end-package

\ maki/tensor.f - nominal tensor shape, dtype, and address-space facts.
\
\ The foundation of the maki tensor type: typed 2D shape arithmetic and the
\ sm_87 dtype set. Pure checked Habu metadata, runnable, independent of the
\ (type-only) PTX runtime. maki -> habu only; maki owns its own error range
\ (-5000..-5099); it never extends lib/errors.f (the one-way fence).
\
\ Shape facts are nominal (Model-CAD V2 R3): rows, cols, dim, and address-space
\ are package CAD-KIND arity-0 families, so a transposed constructor call or a
\ row/col swap is a checker reject, not a latent bug. The private *-REFINE /
\ *-RAW pairs below are the only raw-representation authority; public
\ constructors validate first (SHAPE rejects negative extents).

require maki/cad-kinds.f

\ -5000 (E-MK-DTYPE) retired: the dtype family makes an out-of-range tag a
\ checker reject; the code stays reserved to tensor.
-5006 constant E-MK-DIM        \ negative or overflowing dimension arithmetic
\ -5007 (master's E-MK-LAYOUT) reserved: layout is an ENUM family
\ (maki/tensor-value.f), so a bad layout tag is unrepresentable here.
-5008 constant E-MK-SPACE      \ address-space wire code out of range

package MAKI

\ Zero-arity families are runtime cells. These private identity boundaries are
\ the only raw representation authority; public constructors validate first.
TRUSTED: DIM-REFINE ( n -- CAD-KIND:dim ) ;
TRUSTED: DIM-RAW ( CAD-KIND:dim -- n ) ;
TRUSTED: ROWS-REFINE ( n -- CAD-KIND:rows ) ;
TRUSTED: ROWS-RAW ( CAD-KIND:rows -- n ) ;
TRUSTED: COLS-REFINE ( n -- CAD-KIND:cols ) ;
TRUSTED: COLS-RAW ( CAD-KIND:cols -- n ) ;
TRUSTED: SPACE-REFINE ( n -- CAD-KIND:address-space ) ;
TRUSTED: SPACE-RAW ( CAD-KIND:address-space -- n ) ;

$7FFFFFFFFFFFFFFF constant DIM-MAX-N
4 constant SPACE-N

: NONNEG ( n -- n )
   dup 0 < if E-MK-DIM throw then ;

: RANGE ( n n n -- n )
   {: value:n limit:n code:n :}
   value 0 < value limit >= or if code throw then
   value ;

: MUL-DIM ( n n -- n )
   {: a:n b:n :}
   a NONNEG drop
   b NONNEG drop
   a 0= if 0 exit then
   b DIM-MAX-N a / > if E-MK-DIM throw then
   a b * ;

: SUM-DIM ( n n -- n )
   {: a:n b:n :}
   a NONNEG drop
   b NONNEG drop
   a DIM-MAX-N b - > if E-MK-DIM throw then
   a b + ;

: SUB-DIM ( n n -- n )
   {: a:n b:n :}
   a NONNEG drop
   b NONNEG drop
   a b < if E-MK-DIM throw then
   a b - ;

: SAME-DIM? ( n n -- bool )
   = ;

: BCAST-DIM? ( n n -- bool )
   {: a:n b:n :}
   a b = a 1 = or b 1 = or ;

: MAX-DIM ( n n -- n )
   {: a:n b:n :}
   a b > if a else b then ;

public

: SHAPE ( n n -- CAD-KIND:rows CAD-KIND:cols )
   {: rows:n cols:n :}
   rows NONNEG ROWS-REFINE
   cols NONNEG COLS-REFINE ;

: UNIT-ROWS ( -- CAD-KIND:rows ) 1 ROWS-REFINE ;
: UNIT-COLS ( -- CAD-KIND:cols ) 1 COLS-REFINE ;
: ZERO-ROWS ( -- CAD-KIND:rows ) 0 ROWS-REFINE ;

: ADDRESS-SPACE-DECODE ( n -- CAD-KIND:address-space )
   SPACE-N E-MK-SPACE RANGE SPACE-REFINE ;

: SPACE-HOST ( -- CAD-KIND:address-space ) 0 SPACE-REFINE ;
: SPACE-GLOBAL ( -- CAD-KIND:address-space ) 1 SPACE-REFINE ;
: SPACE-SHARED ( -- CAD-KIND:address-space ) 2 SPACE-REFINE ;
: SPACE-LOCAL ( -- CAD-KIND:address-space ) 3 SPACE-REFINE ;

: ADDRESS-SPACE-VALID? ( n -- bool )
   {: value:n :}
   value 0 < 0= value SPACE-N < and ;

\ Element dtypes (Orin sm_87: bf16/f16 yes, no fp8). The `dtype` ENUM is the
\ semantic type carried through construction, Model IR storage, and every
\ consumer (dot habu-cad-adt-swap, corrected plan). The DT-* codes remain ONLY
\ as the wire/hash vocabulary crossed at the named boundaries below; no internal
\ API takes or returns a raw dtype code. Variant tails are df-prefixed (`f32` is
\ a reserved variant tail); DT-KEY renders the wire strings "f32".."i32".
\ DERIVE eq generates the typed identity compare MAKI-DTYPE:EQ ( dtype dtype --
\ bool ) (derive S1) so dtype can be an enum FIELD of a DERIVE-eq PRODUCT (the
\ SKEY schedule key, dot habu-cad-adt-swap); zero behavior change to the codes.
\ The dtype role deliberately stays an ENUM rather than a CAD-KIND nominal
\ (merge policy habu-merge-policy-master-961bb2b7): variant-closed constructors
\ and MATCH renders subsume DTYPE-REFINE/DTYPE-RAW with no raw-n surface.
0 constant DT-F32   1 constant DT-F16   2 constant DT-BF16
3 constant DT-U32   4 constant DT-I32
ENUM dtype DERIVE eq
  df32
  df16
  dbf16
  du32
  di32
;ENUM

\ named render boundaries: dtype -> wire code / wire text (exhaustive MATCH;
\ a bad dtype is unrepresentable, so no throw arm exists)
: DTYPE>N ( dtype -- n )
   MATCH dtype
      df32  OF DT-F32  ENDOF
      df16  OF DT-F16  ENDOF
      dbf16 OF DT-BF16 ENDOF
      du32  OF DT-U32  ENDOF
      di32  OF DT-I32  ENDOF
   ;MATCH ;

: DT-KEY ( dtype -- ptr u8 n )
   MATCH dtype
      df32  OF s" f32"  ENDOF
      df16  OF s" f16"  ENDOF
      dbf16 OF s" bf16" ENDOF
      du32  OF s" u32"  ENDOF
      di32  OF s" i32"  ENDOF
   ;MATCH ;

: DT-SIZE ( dtype -- CAD-KIND:dim )
   MATCH dtype
      df32  OF 4 DIM-REFINE ENDOF
      df16  OF 2 DIM-REFINE ENDOF
      dbf16 OF 2 DIM-REFINE ENDOF
      du32  OF 4 DIM-REFINE ENDOF
      di32  OF 4 DIM-REFINE ENDOF
   ;MATCH ;

\ ---- typed dimension algebra ------------------------------------------------
: DIM* ( CAD-KIND:dim CAD-KIND:dim -- CAD-KIND:dim )
   DIM-RAW swap DIM-RAW swap MUL-DIM DIM-REFINE ;

: ROWS+ ( CAD-KIND:rows CAD-KIND:rows -- CAD-KIND:rows )
   ROWS-RAW swap ROWS-RAW swap SUM-DIM ROWS-REFINE ;

: COLS+ ( CAD-KIND:cols CAD-KIND:cols -- CAD-KIND:cols )
   COLS-RAW swap COLS-RAW swap SUM-DIM COLS-REFINE ;

: ROWS- ( CAD-KIND:rows CAD-KIND:rows -- CAD-KIND:rows )
   ROWS-RAW swap ROWS-RAW swap SUB-DIM ROWS-REFINE ;

: COLS- ( CAD-KIND:cols CAD-KIND:cols -- CAD-KIND:cols )
   COLS-RAW swap COLS-RAW swap SUB-DIM COLS-REFINE ;

: DIM-BCAST? ( CAD-KIND:dim CAD-KIND:dim -- bool )
   DIM-RAW swap DIM-RAW swap BCAST-DIM? ;

: DIM-EQUAL? ( CAD-KIND:dim CAD-KIND:dim -- bool )
   DIM-RAW swap DIM-RAW swap SAME-DIM? ;

: DIM-IS? ( CAD-KIND:dim n -- bool )
   {: value:CAD-KIND:dim expected:n :}
   value DIM-RAW expected SAME-DIM? ;

: ROWS-EQUAL? ( CAD-KIND:rows CAD-KIND:rows -- bool )
   ROWS-RAW swap ROWS-RAW swap SAME-DIM? ;

: COLS-EQUAL? ( CAD-KIND:cols CAD-KIND:cols -- bool )
   COLS-RAW swap COLS-RAW swap SAME-DIM? ;

: ROWS-IS? ( CAD-KIND:rows n -- bool )
   {: value:CAD-KIND:rows expected:n :}
   value ROWS-RAW expected SAME-DIM? ;

: COLS-IS? ( CAD-KIND:cols n -- bool )
   {: value:CAD-KIND:cols expected:n :}
   value COLS-RAW expected SAME-DIM? ;

: ADDRESS-SPACE-EQUAL? ( CAD-KIND:address-space CAD-KIND:address-space -- bool )
   SPACE-RAW swap SPACE-RAW swap SAME-DIM? ;

: DIM-MAX ( CAD-KIND:dim CAD-KIND:dim -- CAD-KIND:dim )
   DIM-RAW swap DIM-RAW swap MAX-DIM DIM-REFINE ;

: SHAPE-ELEMS ( CAD-KIND:rows CAD-KIND:cols -- CAD-KIND:dim )
   COLS-RAW swap ROWS-RAW swap MUL-DIM DIM-REFINE ;

: SHAPE-BCAST? ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- bool )
   {: r1:CAD-KIND:rows c1:CAD-KIND:cols r2:CAD-KIND:rows c2:CAD-KIND:cols :}
   r1 ROWS-RAW r2 ROWS-RAW BCAST-DIM?
   c1 COLS-RAW c2 COLS-RAW BCAST-DIM? and ;

: TENSOR-BYTES ( CAD-KIND:rows CAD-KIND:cols dtype -- CAD-KIND:dim )
   DT-SIZE {: es:CAD-KIND:dim :}
   SHAPE-ELEMS es DIM* ;

: SHAPE-EQUAL? ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- bool )
   {: r1:CAD-KIND:rows c1:CAD-KIND:cols r2:CAD-KIND:rows c2:CAD-KIND:cols :}
   r1 ROWS-RAW r2 ROWS-RAW SAME-DIM?
   c1 COLS-RAW c2 COLS-RAW SAME-DIM? and ;

: SHAPE-IS? ( CAD-KIND:rows CAD-KIND:cols n n -- bool )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols expected-rows:n expected-cols:n :}
   rows ROWS-RAW expected-rows SAME-DIM?
   cols COLS-RAW expected-cols SAME-DIM? and ;

: BCAST-SHAPE ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- CAD-KIND:rows CAD-KIND:cols )
   {: r1:CAD-KIND:rows c1:CAD-KIND:cols r2:CAD-KIND:rows c2:CAD-KIND:cols :}
   r1 c1 r2 c2 SHAPE-BCAST? 0= if E-MK-DIM throw then
   r1 ROWS-RAW r2 ROWS-RAW MAX-DIM ROWS-REFINE
   c1 COLS-RAW c2 COLS-RAW MAX-DIM COLS-REFINE ;

: INNER-EQUAL? ( CAD-KIND:cols CAD-KIND:rows -- bool )
   ROWS-RAW swap COLS-RAW swap SAME-DIM? ;

: TRANSPOSE-SHAPE ( CAD-KIND:rows CAD-KIND:cols -- CAD-KIND:rows CAD-KIND:cols )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   cols COLS-RAW ROWS-REFINE
   rows ROWS-RAW COLS-REFINE ;

\ owner-sanctioned role conversion: an element count reused as a row extent
\ (a gather's output rows = its index tensor's elems); nonneg by construction.
: DIM>ROWS ( CAD-KIND:dim -- CAD-KIND:rows )
   DIM-RAW ROWS-REFINE ;

\ owner-sanctioned slice extent: the [a,b) row window of a source extent.
\ SUB-DIM rejects a<0 / b<a; the source bound rejects b>rows (E-MK-DIM).
: ROWS-WINDOW ( n n CAD-KIND:rows -- CAD-KIND:rows )
   {: a:n b:n src:CAD-KIND:rows :}
   b src ROWS-RAW > if E-MK-DIM throw then
   b a SUB-DIM ROWS-REFINE ;

;package

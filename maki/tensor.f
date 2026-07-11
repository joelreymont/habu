\ maki/tensor.f - tensor shape + dtype metadata (maki v0 tensor type).
\
\ The foundation of the maki tensor type: 2D shape arithmetic and the sm_87 dtype
\ set. Pure checked Habu metadata, runnable, independent of the (type-only) PTX
\ runtime - so it is the natural first maki component. maki -> habu only: this
\ file needs no Habu library beyond core. Maki owns its own error range
\ (-5000..-5099); it never extends lib/errors.f (the one-way fence).

\ -5000 (E-MK-DTYPE) retired: the dtype family makes an out-of-range tag a
\ checker reject; the code stays reserved to tensor.

package MAKI
public

\ Element dtypes (Orin sm_87: bf16/f16 yes, no fp8). The `dtype` ENUM is the
\ semantic type carried through construction, Model IR storage, and every
\ consumer (dot habu-cad-adt-swap, corrected plan). The DT-* codes remain ONLY
\ as the wire/hash vocabulary crossed at the named boundaries below; no internal
\ API takes or returns a raw dtype code. Variant tails are df-prefixed (`f32` is
\ a reserved variant tail); DT-KEY renders the wire strings "f32".."i32".
\ DERIVE eq generates the typed identity compare MAKI-DTYPE:EQ ( dtype dtype --
\ bool ) (derive S1) so dtype can be an enum FIELD of a DERIVE-eq PRODUCT (the
\ SKEY schedule key, dot habu-cad-adt-swap); zero behavior change to the codes.
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

: DT-SIZE ( dtype -- n )
   MATCH dtype
      df32  OF 4 ENDOF
      df16  OF 2 ENDOF
      dbf16 OF 2 ENDOF
      du32  OF 4 ENDOF
      di32  OF 4 ENDOF
   ;MATCH ;

\ A v0 tensor shape is 2D: ( rows cols ) on the stack.
: SHAPE-ELEMS ( n n -- n )  * ;

\ Two dims broadcast iff equal or either is 1 (NumPy rule).
: DIM-BCAST? ( n n -- bool ) {: a b :}
   a b =  a 1 =  or  b 1 =  or ;

: SHAPE-BCAST? ( n n n n -- bool ) {: r1 c1 r2 c2 :}
   r1 r2 DIM-BCAST?  c1 c2 DIM-BCAST?  and ;

: TENSOR-BYTES ( n n dtype -- n )      \ rows cols dtype -- bytes
   DT-SIZE {: es:n :}  SHAPE-ELEMS es * ;

: SHAPE-EQUAL? ( n n n n -- bool ) {: r1 c1 r2 c2 :}
   r1 r2 =  c1 c2 =  and ;

\ Broadcast result dim: for compatible dims (equal, or one is 1) the non-1 wins,
\ which is max. Pair with SHAPE-BCAST? to guard compatibility first.
: DIM-MAX ( n n -- n ) {: a b :}
   a b > if a else b then ;

: BCAST-SHAPE ( n n n n -- n n ) {: r1 c1 r2 c2 :}
   r1 r2 DIM-MAX  c1 c2 DIM-MAX ;

end-package

\ maki/tensor.f - tensor shape + dtype metadata (maki v0 tensor type).
\
\ The foundation of the maki tensor type: 2D shape arithmetic and the sm_87 dtype
\ set. Pure checked Habu metadata, runnable, independent of the (type-only) PTX
\ runtime - so it is the natural first maki component. maki -> habu only: this
\ file needs no Habu library beyond core. Maki owns its own error range
\ (-5000..-5099); it never extends lib/errors.f (the one-way fence).

-5000 constant E-MK-DTYPE   \ dtype tag out of range

package MAKI
public

\ Element dtypes (Orin sm_87: bf16/f16 yes, no fp8).
0 constant DT-F32   1 constant DT-F16   2 constant DT-BF16
3 constant DT-U32   4 constant DT-I32
5 constant DT-N             \ number of dtypes (range bound)

\ Real ENUM co-located with the DT-* codes (dot habu-cad-adt-swap; capability S1).
\ DT-* stay the public/wire/table-index vocabulary; the model IR stores this enum
\ behind the unchanged n accessors and converts at model-ir.f's boundary words
\ (>DTYPE / DTYPE>N / DT-KEY). Variant tails are df-prefixed: `f32` is a reserved
\ enum-variant tail. Declaration order tracks DT-F32..DT-I32.
ENUM dtype
  df32
  df16
  dbf16
  du32
  di32
;ENUM

: DT-VALID? ( n -- bool ) {: dt :}
   dt 0 < 0=  dt DT-N <  and ;

: DT-SIZE ( n -- n ) {: dt :}
   dt case
      DT-F32  of 4 endof
      DT-F16  of 2 endof
      DT-BF16 of 2 endof
      DT-U32  of 4 endof
      DT-I32  of 4 endof
      E-MK-DTYPE throw
   endcase ;

\ A v0 tensor shape is 2D: ( rows cols ) on the stack.
: SHAPE-ELEMS ( n n -- n )  * ;

\ Two dims broadcast iff equal or either is 1 (NumPy rule).
: DIM-BCAST? ( n n -- bool ) {: a b :}
   a b =  a 1 =  or  b 1 =  or ;

: SHAPE-BCAST? ( n n n n -- bool ) {: r1 c1 r2 c2 :}
   r1 r2 DIM-BCAST?  c1 c2 DIM-BCAST?  and ;

: TENSOR-BYTES ( n n n -- n ) {: rows cols dt :}
   rows cols SHAPE-ELEMS  dt DT-SIZE  * ;

: SHAPE-EQUAL? ( n n n n -- bool ) {: r1 c1 r2 c2 :}
   r1 r2 =  c1 c2 =  and ;

\ Broadcast result dim: for compatible dims (equal, or one is 1) the non-1 wins,
\ which is max. Pair with SHAPE-BCAST? to guard compatibility first.
: DIM-MAX ( n n -- n ) {: a b :}
   a b > if a else b then ;

: BCAST-SHAPE ( n n n n -- n n ) {: r1 c1 r2 c2 :}
   r1 r2 DIM-MAX  c1 c2 DIM-MAX ;

end-package

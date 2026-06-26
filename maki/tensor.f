\ maki/tensor.f - tensor shape + dtype metadata (maki v0 tensor type).
\
\ The foundation of the maki tensor type: 2D shape arithmetic and the sm_87 dtype
\ set. Pure checked Habu metadata, runnable, independent of the (type-only) PTX
\ runtime - so it is the natural first maki component. maki -> habu only: this
\ file needs no Habu library beyond core. Maki owns its own error range
\ (-5000..-5099); it never extends lib/errors.f (the one-way fence).

-5000 constant E-MK-DTYPE   \ dtype tag out of range

\ Element dtypes (Orin sm_87: bf16/f16 yes, no fp8).
0 constant DT-F32   1 constant DT-F16   2 constant DT-BF16
3 constant DT-U32   4 constant DT-I32
5 constant DT-N             \ number of dtypes (range bound)

create DT-SIZES  4 , 2 , 2 , 4 , 4 ,    \ bytes per dtype, indexed by DT-*

: DT-VALID? ( n -- bool ) {: dt :}
   dt 0 < 0=  dt DT-N <  and ;

: DT-SIZE ( n -- n ) {: dt :}
   dt DT-VALID? 0= if E-MK-DTYPE throw then
   dt cells DT-SIZES + @ ;

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

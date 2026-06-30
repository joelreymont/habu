\ float-cell.f - the single explicit trusted boundary for storing floats in cells.
\ Habu cells are generic; @/! move an opaque cell and the checker silently coerces
\ it to/from r at use. F@/F! make that reinterpret explicit and asserted ONCE here
\ (TRUSTED:), so float-array code never takes an implicit a<->r boundary by default.
\ A float array is a `ptr r` (cell-addressed); F@ fetches the r at an address, F!
\ stores one. Load before any odin module that keeps floats in cell arrays. The
\ live detector also needs to widen the thin C ABI's raw f32 TensorRT buffer into
\ Habu's f64 cells before calling the checked YOLO decoder; the f32 bit read and
\ bit reinterpret stay here as the one audited boundary.

TRUSTED: F@ ( ptr a -- r ) @ ;          \ fetch a float from a cell address
TRUSTED: F! ( r ptr a -- ) ! ;          \ store a float to a cell address
TRUSTED: FC-BITS>R ( n -- r ) ;         \ reinterpret an IEEE-754 f64 bit cell

: FC-U32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@  a 1 + c@ 8 lshift or  a 2 + c@ 16 lshift or  a 3 + c@ 24 lshift or ;

: FC-F32>F64 ( n -- r ) {: b:n :}
   b 31 rshift 1 and {: sgn:n :}
   b 23 rshift $FF and {: e32:n :}
   b $7FFFFF and {: m32:n :}
   sgn 63 lshift {: hi:n :}
   e32 0= if hi FC-BITS>R exit then
   e32 $FF = if hi $7FF 52 lshift or FC-BITS>R exit then
   hi e32 896 + 52 lshift or m32 29 lshift or FC-BITS>R ;

: FC-F32@ ( ptr u8 -- r )
   FC-U32@ FC-F32>F64 ;

: FC-F32-I@ ( ptr u8 n -- r ) {: raw:ptr idx:n :}
   raw idx 4 * + FC-F32@ ;

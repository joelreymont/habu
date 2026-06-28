\ c3-widen-test.f - C3 narrow->wide int widening (positive). Nominal roles + ptr
\ stay strict (negative proven: ': C3BAD ( ptr u8 -- ) <i64-word> ;' is rejected
\ with "expected: i64 actual: ptr u8").
: C3F ( i64 -- ) drop ;
: C3WU8  ( u8 -- )  C3F ;      \ u8  widens to i64
: C3WU32 ( u32 -- ) C3F ;      \ u32 widens to i64
: C3RUN ( -- ) 7 C3WU8  9 C3WU32 ;
C3RUN
s" c3-widen ok (u8/u32 -> i64; ptr/nominal stay strict)" type cr

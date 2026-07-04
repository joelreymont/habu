\ ptx-test.f - checked PTX header vocabulary tests.

require lib/ptx/test-prelude.f

T-RESET

256 %BLOCK
PTX-BLOCK@ 256 T=

: PTX-BAD-BLOCK-0 ( -- )
   0 %BLOCK ;
: PTX-BAD-BLOCK-33 ( -- )
   33 %BLOCK ;
: PTX-BAD-BLOCK-1056 ( -- )
   1056 %BLOCK ;
' PTX-BAD-BLOCK-0 E-PTX-BLOCK TTHROWS
' PTX-BAD-BLOCK-33 E-PTX-BLOCK TTHROWS
' PTX-BAD-BLOCK-1056 E-PTX-BLOCK TTHROWS

KERNEL: PTX-TEST-K1 ( n -- n ) GRID: once
   1+ ;
4 PTX-TEST-K1 5 T=

KERNEL: PTX-TEST-K2 ( n -- n ) GRID: once WHERE extent-n <= block-256
   1+ ;
6 PTX-TEST-K2 7 T=

: PTX-BAD-WHERE-LHS ( -- )
   s" n" s" <=" s" block-256" PTX-WHERE-CHECK ;
: PTX-BAD-WHERE-OP ( -- )
   s" extent-n" s" <" s" block-256" PTX-WHERE-CHECK ;
: PTX-BAD-WHERE-RHS ( -- )
   s" extent-n" s" <=" s" block-x" PTX-WHERE-CHECK ;
: PTX-BAD-WHERE-BLOCK ( -- )
   s" extent-n" s" <=" s" block-1024" PTX-WHERE-CHECK ;
' PTX-BAD-WHERE-LHS E-PTX-SYNTAX TTHROWS
' PTX-BAD-WHERE-OP E-PTX-SYNTAX TTHROWS
' PTX-BAD-WHERE-RHS E-PTX-SYNTAX TTHROWS
' PTX-BAD-WHERE-BLOCK E-PTX-BLOCK TTHROWS

\ f64 -> f32 marshalling (lib/ptx/cg.f): exact for normal values
3.0 F64>F32 1077936128 T=     \ 0x40400000
2.0 F64>F32 1073741824 T=     \ 0x40000000
6.0 F64>F32 1086324736 T=     \ 0x40C00000
0.0 F64>F32          0 T=

\ F64>F32 round-to-nearest-even + IEEE specials
1.0 F64>F32 $3F800000 T=                          \ exact
1.7 F64>F32 $3FD9999A T=                          \ 1.7 has no exact f32: rounds up
1 63 lshift BITS>R F64>F32 $80000000 T=           \ -0.0 keeps its sign
$3FF0000010000000 BITS>R F64>F32 $3F800000 T=     \ 1 + 2^-24: exact tie -> even (down)
$3FF0000030000000 BITS>R F64>F32 $3F800002 T=     \ 1 + 3*2^-24: exact tie -> odd (up)
$47F0000000000000 BITS>R F64>F32 $7F800000 T=     \ 2^128: overflow -> +inf
1 63 lshift $47F0000000000000 or BITS>R F64>F32 $FF800000 T=   \ -2^128: overflow -> -inf
$7FF8000000000000 BITS>R F64>F32 $7FC00000 T=     \ quiet NaN preserved quiet
$7FF0000000000001 BITS>R F64>F32 $7FC00000 T=     \ signaling NaN quieted

\ f32 array pack / unpack (device-upload marshalling)
create PK-SRC  4 cells allot
create PK-DST  16 allot
create PK-BACK 4 cells allot
1.0 PK-SRC 0 cells + !   1.7 PK-SRC 1 cells + !
2.0 PK-SRC 2 cells + !   0.5 PK-SRC 3 cells + !
PK-SRC 4 PK-DST F32-PACK
PK-DST      SF-LD $3F800000 T=                    \ 1.0
PK-DST 4  + SF-LD $3FD9999A T=                    \ 1.7 (rounded)
PK-DST 8  + SF-LD $40000000 T=                    \ 2.0
PK-DST 12 + SF-LD $3F000000 T=                    \ 0.5
PK-DST 4 PK-BACK F32-UNPACK                       \ widen back; re-narrow == original
PK-BACK 0 cells + @ F64>F32 $3F800000 T=
PK-BACK 1 cells + @ F64>F32 $3FD9999A T=
PK-BACK 2 cells + @ F64>F32 $40000000 T=
PK-BACK 3 cells + @ F64>F32 $3F000000 T=

T-REPORT

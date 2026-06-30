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

T-REPORT

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

\ GRID: records its derivation token (queryable via PTX-GRID$, last header wins)
PTX-GRID$ s" once" T$=
GRID: ceil-n-256
PTX-GRID$ s" ceil-n-256" T$=
GRID: extent-r
PTX-GRID$ s" extent-r" T$=

: PTX-GRID-LONG ( -- )
   s" a-token-longer-than-the-sixty-four-byte-grid-capacity-limit-xxxxxxxxxx" PTX-GRID! ;
' PTX-GRID-LONG E-PTX-SYNTAX TTHROWS
PTX-GRID$ s" extent-r" T$=              \ rejected store leaves the recorded token intact

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

\ The legacy PTX spellings remain exact wrappers over the package-owned codec.
1.7 F64>F32 $3FD9999A T=
$00000001 F32>F64 F64>F32 $00000001 T=

\ Existing PTX-owned raw marshalling remains covered at its compatibility boundary.
create PK-SRC  4 cells allot
create PK-DST  16 allot
create PK-BACK 4 cells allot
1.0 PK-SRC 0 cells + !   1.7 PK-SRC 1 cells + !
2.0 PK-SRC 2 cells + !   0.5 PK-SRC 3 cells + !
PK-SRC 4 PK-DST F32-PACK
PK-DST      SF-LD $3F800000 T=
PK-DST 4  + SF-LD $3FD9999A T=
PK-DST 8  + SF-LD $40000000 T=
PK-DST 12 + SF-LD $3F000000 T=
PK-DST 4 PK-BACK F32-UNPACK
PK-BACK 0 cells + @ F64>F32 $3F800000 T=
PK-BACK 1 cells + @ F64>F32 $3FD9999A T=
PK-BACK 2 cells + @ F64>F32 $40000000 T=
PK-BACK 3 cells + @ F64>F32 $3F000000 T=

\ The PTX F16 and BF16 packers are real consumers of the shared IEEE-754
\ round-to-nearest-even shift. Pin exact ties and gradual-underflow boundaries
\ here so changing the generic helper cannot silently alter device operands.
1.0 F64>F16 $3C00 T=
$3FF0020000000000 IEEE754:BITS>F64 F64>F16 $3C00 T=
$3FF0060000000000 IEEE754:BITS>F64 F64>F16 $3C02 T=
$3E70000000000000 IEEE754:BITS>F64 F64>F16 $0001 T=
$3E60000000000000 IEEE754:BITS>F64 F64>F16 $0000 T=
$3E78000000000000 IEEE754:BITS>F64 F64>F16 $0002 T=

1.0 F64>BF16 $3F80 T=
$3FF0100000000000 IEEE754:BITS>F64 F64>BF16 $3F80 T=
$3FF0300000000000 IEEE754:BITS>F64 F64>BF16 $3F82 T=
$37A0000000000000 IEEE754:BITS>F64 F64>BF16 $0001 T=
$3790000000000000 IEEE754:BITS>F64 F64>BF16 $0000 T=
$37A8000000000000 IEEE754:BITS>F64 F64>BF16 $0002 T=

T-REPORT

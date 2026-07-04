\ maki/tensor-value-test.f - checked tests for the unified single-slot tensor
\ value, its recorded facts, the descriptor-mode plan wordset, and eager interop.
\ Test-local names are TVT- prefixed: suites share the MAKI wordlist.

require lib/test.f
require maki/tensor-value.f

package MAKI

T-RESET

\ ---- shared data buffer (values are irrelevant to construction/facts) -------
create TVT-BX 4 cells allot
1.0 TVT-BX 0 T-SET  2.0 TVT-BX 1 T-SET  3.0 TVT-BX 2 T-SET  4.0 TVT-BX 3 T-SET

\ create-space is byte-aligned, so round TVT-BX up to a known 16-byte boundary to
\ get a deterministic anchor for the alignment tests (P>N measures the low bits).
: TVT-A16 ( -- ptr a )  TVT-BX  TVT-BX P>N 15 and  16 swap -  15 and  + ;

\ ---- default constructor: f32 + row-major, materialized, data round-trips ---
TVT-BX 2 2 TV-NEW
dup TV-ROWS@    2       T=
dup TV-COLS@    2       T=
dup TV-DTYPE@   DT-F32  T=
dup TV-LAYOUT@  LAY-ROW T=
dup TV-ELEMS    4       T=
dup TV-HAS-DATA?        TTRUE
dup TV-DATA@ TVT-BX =   TTRUE
drop

\ ---- explicit constructor + settable dtype/layout ---------------------------
TVT-BX 3 5 DT-BF16 LAY-COL TV-NEW-AS
dup TV-ROWS@    3       T=
dup TV-COLS@    5       T=
dup TV-DTYPE@   DT-BF16 T=
dup TV-LAYOUT@  LAY-COL T=
dup DT-F16  TV-DTYPE!  TV-DTYPE@  DT-F16  T=
dup LAY-ROW TV-LAYOUT! TV-LAYOUT@ LAY-ROW T=
drop

\ ---- alignment recorded from the actual pointer -----------------------------
\ Exact class from a known 16-aligned anchor and byte offsets off it.
TVT-A16     1 1 TV-NEW  dup TV-ALIGN@ AL-16   T=  drop
TVT-A16 8 + 1 1 TV-NEW  dup TV-ALIGN@ AL-8    T=  drop
TVT-A16 4 + 1 1 TV-NEW  dup TV-ALIGN@ AL-4    T=  drop
TVT-A16 2 + 1 1 TV-NEW  dup TV-ALIGN@ AL-BYTE T=  drop
TVT-A16 1 + 1 1 TV-NEW  dup TV-ALIGN@ AL-BYTE T=  drop

\ ---- descriptor: shape/dtype only, no buffer, conservative alignment --------
2 3 DT-F32 LAY-ROW TV-DESC
dup TV-ROWS@   2          T=
dup TV-COLS@   3          T=
dup TV-DTYPE@  DT-F32     T=
dup TV-ALIGN@  AL-UNKNOWN T=
dup TV-HAS-DATA?          TFALSE
drop

\ ---- eager interop: TV-LINEAR == eager LINEAR (linear-test numbers) ---------
\ X=[[1,2],[3,4]] W=[[5,6],[7,8]] b=[100,200] -> Y=[[119,222],[143,250]].
create TVT-IX 4 cells allot   create TVT-IW 4 cells allot   create TVT-IB 2 cells allot
create TVT-IY 4 cells allot   create TVT-IYE 4 cells allot
1.0 TVT-IX 0 T-SET  2.0 TVT-IX 1 T-SET  3.0 TVT-IX 2 T-SET  4.0 TVT-IX 3 T-SET
5.0 TVT-IW 0 T-SET  6.0 TVT-IW 1 T-SET  7.0 TVT-IW 2 T-SET  8.0 TVT-IW 3 T-SET
100.0 TVT-IB 0 T-SET  200.0 TVT-IB 1 T-SET

TVT-IX TVT-IW TVT-IB TVT-IYE 2 2 2 LINEAR              \ eager reference into TVT-IYE
TVT-IX 2 2 TV-NEW  TVT-IW 2 2 TV-NEW  TVT-IB 1 2 TV-NEW  TVT-IY 2 2 TV-NEW  TV-LINEAR drop

TVT-IY 0 T-GET TVT-IYE 0 T-GET f- f>s 0 T=             \ identical, element-wise
TVT-IY 1 T-GET TVT-IYE 1 T-GET f- f>s 0 T=
TVT-IY 2 T-GET TVT-IYE 2 T-GET f- f>s 0 T=
TVT-IY 3 T-GET TVT-IYE 3 T-GET f- f>s 0 T=
TVT-IY 0 T-GET 0.5 f+ f>s 119 T=                       \ and matches the closed form
TVT-IY 1 T-GET 0.5 f+ f>s 222 T=
TVT-IY 3 T-GET 0.5 f+ f>s 250 T=

\ ---- plan builder mechanism (PLAN-RESET / begin / IN+ / OP+ / accessors) ----
variable TVT-BLDX
PLAN-RESET
2 3 DT-F32 LAY-ROW TV-DESC TVT-BLDX !
OP-GELU PLAN-OP-BEGIN
TVT-BLDX @ PLAN-IN+
2 3 DT-F32 LAY-ROW TV-DESC PLAN-OP+
PLAN-N@ 1 T=
0 PLAN-OP@ OP-GELU T=
0 PLAN-IN-COUNT@ 1 T=
0 0 PLAN-IN@ tensor>N  TVT-BLDX @ tensor>N  T=

\ ---- descriptor-mode model: PLINEAR PGELU PLINEAR records the right sequence -
variable TVT-X  variable TVT-W1  variable TVT-B1  variable TVT-W2  variable TVT-B2
PLAN-RESET
2 3 DT-F32 LAY-ROW TV-DESC TVT-X !                     \ X   2x3
3 4 DT-F32 LAY-ROW TV-DESC TVT-W1 !                    \ W1  3x4
1 4 DT-F32 LAY-ROW TV-DESC TVT-B1 !                    \ b1  1x4
4 5 DT-F32 LAY-ROW TV-DESC TVT-W2 !                    \ W2  4x5
1 5 DT-F32 LAY-ROW TV-DESC TVT-B2 !                    \ b2  1x5
TVT-X @ TVT-W1 @ TVT-B1 @ PLINEAR  PGELU  TVT-W2 @ TVT-B2 @ PLINEAR  drop

PLAN-N@ 3 T=
0 PLAN-OP@ OP-LINEAR T=
1 PLAN-OP@ OP-GELU   T=
2 PLAN-OP@ OP-LINEAR T=
0 PLAN-IN-COUNT@ 3 T=
1 PLAN-IN-COUNT@ 1 T=
2 PLAN-IN-COUNT@ 3 T=
0 0 PLAN-IN@ tensor>N  TVT-X @  tensor>N T=            \ op0 inputs = X W1 b1
0 1 PLAN-IN@ tensor>N  TVT-W1 @ tensor>N T=
0 2 PLAN-IN@ tensor>N  TVT-B1 @ tensor>N T=
1 0 PLAN-IN@ tensor>N  0 PLAN-OUT@ tensor>N T=         \ GELU consumes op0 output
2 0 PLAN-IN@ tensor>N  1 PLAN-OUT@ tensor>N T=         \ op2 consumes GELU output
2 1 PLAN-IN@ tensor>N  TVT-W2 @ tensor>N T=
0 PLAN-OUT@ TV-ROWS@ 2 T=                              \ inferred output shapes
0 PLAN-OUT@ TV-COLS@ 4 T=
1 PLAN-OUT@ TV-COLS@ 4 T=
2 PLAN-OUT@ TV-ROWS@ 2 T=
2 PLAN-OUT@ TV-COLS@ 5 T=

\ ---- fail-closed probes (top-level cannot push quotations) ------------------
: TVT-BAD-DT    ( -- )  TVT-BX 2 2 99 LAY-ROW TV-NEW-AS drop ;
: TVT-BAD-LAY   ( -- )  TVT-BX 2 2 DT-F32 99 TV-NEW-AS drop ;
: TVT-BAD-DT!   ( -- )  TVT-BX 2 2 TV-NEW 99 TV-DTYPE!  drop ;
: TVT-BAD-LAY!  ( -- )  TVT-BX 2 2 TV-NEW 99 TV-LAYOUT! drop ;
: TVT-NODATA    ( -- )  2 2 DT-F32 LAY-ROW TV-DESC TV-DATA@ drop ;
: TVT-BAD-SHAPE ( -- )  TVT-BX 2 3 TV-NEW  TVT-BX 2 2 TV-NEW  TVT-BX 1 2 TV-NEW  TVT-BX 2 2 TV-NEW  TV-LINEAR drop ;
: TVT-PLAN-IDX  ( -- )  PLAN-RESET 0 PLAN-OP@ drop ;
: TVT-OPKIND    ( -- )  PLAN-RESET 99 PLAN-OP-BEGIN ;
: TVT-PLAN-DBL  ( -- )  PLAN-RESET OP-GELU PLAN-OP-BEGIN OP-GELU PLAN-OP-BEGIN ;
: TVT-IN-NB     ( -- )  PLAN-RESET 2 2 DT-F32 LAY-ROW TV-DESC PLAN-IN+ ;
: TVT-OP-NB     ( -- )  PLAN-RESET 2 2 DT-F32 LAY-ROW TV-DESC PLAN-OP+ ;
: TVT-FULL-TV   ( -- )  TV-RESET  TV-CAP 1+ 0 ?do 2 2 DT-F32 LAY-ROW TV-DESC drop loop ;
: TVT-FULL-PLAN ( -- )
   TV-RESET PLAN-RESET
   PLAN-CAP 1+ 0 ?do  OP-GELU PLAN-OP-BEGIN  2 2 DT-F32 LAY-ROW TV-DESC PLAN-OP+  loop ;

' TVT-BAD-DT    E-MK-DTYPE      TTHROWS
' TVT-BAD-LAY   E-TV-LAYOUT     TTHROWS
' TVT-BAD-DT!   E-MK-DTYPE      TTHROWS
' TVT-BAD-LAY!  E-TV-LAYOUT     TTHROWS
' TVT-NODATA    E-TV-NODATA     TTHROWS
' TVT-BAD-SHAPE E-TV-SHAPE      TTHROWS
' TVT-PLAN-IDX  E-TV-PLAN-IDX   TTHROWS
' TVT-OPKIND    E-TV-OPKIND     TTHROWS
' TVT-PLAN-DBL  E-TV-PLAN-STATE TTHROWS
' TVT-IN-NB     E-TV-PLAN-STATE TTHROWS
' TVT-OP-NB     E-TV-PLAN-STATE TTHROWS
' TVT-FULL-TV   E-TV-FULL       TTHROWS
' TVT-FULL-PLAN E-TV-PLAN-FULL  TTHROWS

T-REPORT

end-package

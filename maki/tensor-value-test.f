\ maki/tensor-value-test.f - nominal tensor descriptor and storage tests
\ (typed shape/space kinds + dtype/layout/align enums + generation handles).
\ Test-local names are TVT- prefixed: suites share the MAKI wordlist.

require lib/test.f
require test/checker-assert.f
require maki/tensor-value.f

package MAKI

T-RESET

\ ---- shared data buffer (values are irrelevant to construction/facts) -------
create TVT-BX 4 cells allot
1.0 TVT-BX 0 T-SET  2.0 TVT-BX 1 T-SET  3.0 TVT-BX 2 T-SET  4.0 TVT-BX 3 T-SET

\ create-space is byte-aligned, so round TVT-BX up to a known 16-byte boundary to
\ get a deterministic anchor for the alignment tests (P>N measures the low bits).
: TVT-A16 ( -- ptr a )  TVT-BX  TVT-BX P>N 15 and  16 swap -  15 and  + ;

: TVT-SHAPE? ( tensor n n -- bool )
   {: t:tensor rows:n cols:n :}
   t TENSOR:TV-ROWS@ t TENSOR:TV-COLS@ rows cols SHAPE-IS? ;

\ ---- default constructor: f32 + row-major + host, materialized --------------
TVT-BX 2 2 SHAPE TENSOR:TV-NEW
dup 2 2 TVT-SHAPE? TTRUE
dup TENSOR:TV-DTYPE@   MAKI-DTYPE:DF32 MAKI-DTYPE:EQ TTRUE
dup TENSOR:TV-LAYOUT@  MAKI-LAYOUT:ROW MAKI-LAYOUT:EQ TTRUE
dup TENSOR:TV-SPACE@   SPACE-HOST ADDRESS-SPACE-EQUAL? TTRUE
dup TENSOR:TV-ELEMS    4 DIM-IS? TTRUE
dup TENSOR:TV-HAS-DATA?        TTRUE
dup TENSOR:TV-DATA@ TVT-BX =   TTRUE
drop

\ ---- explicit host constructor records typed dtype/layout facts -------------
TVT-BX 3 5 SHAPE MAKI-DTYPE:DBF16 MAKI-LAYOUT:COL TENSOR:TV-NEW-HOST
dup 3 5 TVT-SHAPE? TTRUE
dup TENSOR:TV-DTYPE@   DTYPE>N DT-BF16 T=
dup TENSOR:TV-LAYOUT@  LAYOUT>N LAY-COL T=
dup TENSOR:TV-SPACE@   SPACE-HOST ADDRESS-SPACE-EQUAL? TTRUE
drop

\ ---- alignment recorded from the actual pointer -----------------------------
\ Exact class from a known 16-aligned anchor and byte offsets off it.
TVT-A16     1 1 SHAPE TENSOR:TV-NEW  dup TENSOR:TV-ALIGN@ ALIGN>N AL-16   T=  drop
TVT-A16 8 + 1 1 SHAPE TENSOR:TV-NEW  dup TENSOR:TV-ALIGN@ ALIGN>N AL-8    T=  drop
TVT-A16 4 + 1 1 SHAPE TENSOR:TV-NEW  dup TENSOR:TV-ALIGN@ ALIGN>N AL-4    T=  drop
TVT-A16 2 + 1 1 SHAPE TENSOR:TV-NEW  dup TENSOR:TV-ALIGN@ ALIGN>N AL-BYTE T=  drop
TVT-A16 1 + 1 1 SHAPE TENSOR:TV-NEW  dup TENSOR:TV-ALIGN@ ALIGN>N AL-BYTE T=  drop

\ ---- descriptor: typed facts only, no buffer, conservative alignment --------
2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC
dup 2 3 TVT-SHAPE? TTRUE
dup TENSOR:TV-DTYPE@  DTYPE>N DT-F32     T=
dup TENSOR:TV-LAYOUT@ MAKI-LAYOUT:ROW MAKI-LAYOUT:EQ TTRUE
dup TENSOR:TV-SPACE@  SPACE-GLOBAL ADDRESS-SPACE-EQUAL? TTRUE
dup TENSOR:TV-ALIGN@  ALIGN>N AL-UNKNOWN T=
dup TENSOR:TV-HAS-DATA?          TFALSE
drop

\ Bufferless descriptors may represent target spaces (no pointer provenance claimed).
1 1 SHAPE MAKI-DTYPE:DF16 MAKI-LAYOUT:ROW SPACE-SHARED TENSOR:TV-DESC
dup TENSOR:TV-SPACE@ SPACE-SHARED ADDRESS-SPACE-EQUAL? TTRUE
drop

\ ---- eager interop: TV-LINEAR == eager LINEAR (linear-test numbers) ---------
\ X=[[1,2],[3,4]] W=[[5,6],[7,8]] b=[100,200] -> Y=[[119,222],[143,250]].
create TVT-IX 4 cells allot   create TVT-IW 4 cells allot   create TVT-IB 2 cells allot
create TVT-IY 4 cells allot   create TVT-IYE 4 cells allot
1.0 TVT-IX 0 T-SET  2.0 TVT-IX 1 T-SET  3.0 TVT-IX 2 T-SET  4.0 TVT-IX 3 T-SET
5.0 TVT-IW 0 T-SET  6.0 TVT-IW 1 T-SET  7.0 TVT-IW 2 T-SET  8.0 TVT-IW 3 T-SET
100.0 TVT-IB 0 T-SET  200.0 TVT-IB 1 T-SET

TVT-IX TVT-IW TVT-IB TVT-IYE 2 2 2 LINEAR              \ eager reference into TVT-IYE
TVT-IX 2 2 SHAPE TENSOR:TV-NEW
TVT-IW 2 2 SHAPE TENSOR:TV-NEW
TVT-IB 1 2 SHAPE TENSOR:TV-NEW
TVT-IY 2 2 SHAPE TENSOR:TV-NEW
TENSOR:TV-LINEAR drop

TVT-IY 0 T-GET TVT-IYE 0 T-GET f- f>s 0 T=             \ identical, element-wise
TVT-IY 1 T-GET TVT-IYE 1 T-GET f- f>s 0 T=
TVT-IY 2 T-GET TVT-IYE 2 T-GET f- f>s 0 T=
TVT-IY 3 T-GET TVT-IYE 3 T-GET f- f>s 0 T=
TVT-IY 0 T-GET 0.5 f+ f>s 119 T=                       \ and matches the closed form
TVT-IY 1 T-GET 0.5 f+ f>s 222 T=
TVT-IY 3 T-GET 0.5 f+ f>s 250 T=

\ ---- plan builder mechanism (PLAN-RESET / begin / IN+ / OP+ / accessors) ----
variable TVT-BLDX
TENSOR:PLAN-RESET
2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TVT-BLDX !
MAKI-OPKIND:GELU TENSOR:PLAN-OP-BEGIN
TVT-BLDX @ TENSOR:PLAN-IN+
2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TENSOR:PLAN-OP+
TENSOR:PLAN-N@ 1 T=
0 TENSOR:PLAN-OP@ OPKIND>N OP-GELU T=
0 TENSOR:PLAN-IN-COUNT@ 1 T=
0 0 TENSOR:PLAN-IN@ TVT-BLDX @ TENSOR:TV-EQUAL? TTRUE

\ ---- descriptor-mode model: PLINEAR PGELU PLINEAR records the right sequence -
variable TVT-X  variable TVT-W1  variable TVT-B1  variable TVT-W2  variable TVT-B2
TENSOR:PLAN-RESET
2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TVT-X !     \ X   2x3
3 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TVT-W1 !    \ W1  3x4
1 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TVT-B1 !    \ b1  1x4
4 5 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TVT-W2 !    \ W2  4x5
1 5 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TVT-B2 !    \ b2  1x5
TVT-X @ TVT-W1 @ TVT-B1 @ TENSOR:PLINEAR  TENSOR:PGELU  TVT-W2 @ TVT-B2 @ TENSOR:PLINEAR  drop

TENSOR:PLAN-N@ 3 T=
0 TENSOR:PLAN-OP@ OPKIND>N OP-LINEAR T=
1 TENSOR:PLAN-OP@ OPKIND>N OP-GELU   T=
2 TENSOR:PLAN-OP@ OPKIND>N OP-LINEAR T=
0 TENSOR:PLAN-IN-COUNT@ 3 T=
1 TENSOR:PLAN-IN-COUNT@ 1 T=
2 TENSOR:PLAN-IN-COUNT@ 3 T=
0 0 TENSOR:PLAN-IN@ TVT-X @  TENSOR:TV-EQUAL? TTRUE          \ op0 inputs = X W1 b1
0 1 TENSOR:PLAN-IN@ TVT-W1 @ TENSOR:TV-EQUAL? TTRUE
0 2 TENSOR:PLAN-IN@ TVT-B1 @ TENSOR:TV-EQUAL? TTRUE
1 0 TENSOR:PLAN-IN@ 0 TENSOR:PLAN-OUT@ TENSOR:TV-EQUAL? TTRUE   \ GELU consumes op0 output
2 0 TENSOR:PLAN-IN@ 1 TENSOR:PLAN-OUT@ TENSOR:TV-EQUAL? TTRUE   \ op2 consumes GELU output
2 1 TENSOR:PLAN-IN@ TVT-W2 @ TENSOR:TV-EQUAL? TTRUE
0 TENSOR:PLAN-OUT@ 2 4 TVT-SHAPE? TTRUE                      \ inferred output shapes
1 TENSOR:PLAN-OUT@ 2 4 TVT-SHAPE? TTRUE
2 TENSOR:PLAN-OUT@ 2 5 TVT-SHAPE? TTRUE

\ ---- fail-closed probes (top-level cannot push quotations) ------------------
: TVT-NODATA    ( -- )
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC
   TENSOR:TV-DATA@ drop ;
: TVT-BAD-SHAPE ( -- )
   TVT-BX 2 3 SHAPE TENSOR:TV-NEW
   TVT-BX 2 2 SHAPE TENSOR:TV-NEW
   TVT-BX 1 2 SHAPE TENSOR:TV-NEW
   TVT-BX 2 2 SHAPE TENSOR:TV-NEW
   TENSOR:TV-LINEAR drop ;
: TVT-PLAN-IDX  ( -- )  TENSOR:PLAN-RESET 0 TENSOR:PLAN-OP@ drop ;
\ (a bad op-kind tag is a checker reject now - pinned by the opkind negatives
\ below - so the old TVT-OPKIND runtime throw probe is unrepresentable)
: TVT-PLAN-DBL  ( -- )  TENSOR:PLAN-RESET MAKI-OPKIND:GELU TENSOR:PLAN-OP-BEGIN MAKI-OPKIND:GELU TENSOR:PLAN-OP-BEGIN ;
: TVT-IN-NB     ( -- )
   TENSOR:PLAN-RESET
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TENSOR:PLAN-IN+ ;
: TVT-OP-NB     ( -- )
   TENSOR:PLAN-RESET
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TENSOR:PLAN-OP+ ;
: TVT-FULL-TV   ( -- )
   TENSOR:TV-RESET
   TENSOR:TV-CAP 1+ 0 ?do
      2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC drop
   loop ;
: TVT-FULL-PLAN ( -- )
   TENSOR:TV-RESET TENSOR:PLAN-RESET
   TENSOR:PLAN-CAP 1+ 0 ?do
      MAKI-OPKIND:GELU TENSOR:PLAN-OP-BEGIN
      2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TENSOR:PLAN-OP+
   loop ;

\ ---- stale handles fail closed once TV-RESET bumps the generation (R3) ------
: TVT-STALE ( -- )
   TENSOR:TV-RESET
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC {: old:tensor :}
   TENSOR:TV-RESET
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC drop
   old TENSOR:TV-ROWS@ drop ;

: TVT-STALE-PLAN ( -- )
   TENSOR:TV-RESET TENSOR:PLAN-RESET
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC {: x:tensor :}
   MAKI-OPKIND:GELU TENSOR:PLAN-OP-BEGIN x TENSOR:PLAN-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC TENSOR:PLAN-OP+
   TENSOR:TV-RESET
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-GLOBAL TENSOR:TV-DESC drop
   0 0 TENSOR:PLAN-IN@ drop ;

' TVT-NODATA    E-TV-NODATA     TTHROWS
' TVT-BAD-SHAPE E-TV-SHAPE      TTHROWS
' TVT-PLAN-IDX  E-TV-PLAN-IDX   TTHROWS
' TVT-PLAN-DBL  E-TV-PLAN-STATE TTHROWS
' TVT-IN-NB     E-TV-PLAN-STATE TTHROWS
' TVT-OP-NB     E-TV-PLAN-STATE TTHROWS
' TVT-STALE      E-TV-HANDLE    TTHROWS
' TVT-STALE-PLAN E-TV-HANDLE    TTHROWS
' TVT-FULL-TV   E-TV-FULL       TTHROWS
' TVT-FULL-PLAN E-TV-PLAN-FULL  TTHROWS

\ ---- swapped-family negatives (dot habu-cad-adt-swap + Model-CAD V2 R3) ------
\ A bad dtype/layout tag is a CHECKER reject at the constructor boundary, the
\ dtype<->layout swap rejects both ways, the typed rows/cols kinds reject a raw
\ or transposed extent, and no raw-n public handle conversion exists (the old
\ TENSOR:tensor>N inspection cast is gone; identity is TENSOR:TV-EQUAL?).
s" TVT-OK-NEW   ( ptr a CAD-KIND:rows CAD-KIND:cols dtype layout -- tensor ) TENSOR:TV-NEW-HOST" CHECK-QUIET-CANDIDATE! -1 T=
s" TVT-N-ROWS   ( ptr a n n dtype layout -- tensor ) TENSOR:TV-NEW-HOST"                         CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-N-DT     ( ptr a CAD-KIND:rows CAD-KIND:cols n layout -- tensor ) TENSOR:TV-NEW-HOST"     CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-N-LAY    ( ptr a CAD-KIND:rows CAD-KIND:cols dtype n -- tensor ) TENSOR:TV-NEW-HOST"      CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-SWAP-NEW ( ptr a CAD-KIND:rows CAD-KIND:cols layout dtype -- tensor ) TENSOR:TV-NEW-HOST" CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-NEG-ROWS-COLS ( CAD-KIND:cols CAD-KIND:rows dtype layout CAD-KIND:address-space -- tensor ) TENSOR:TV-DESC" CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-NEG-DTYPE-LAYOUT ( CAD-KIND:rows CAD-KIND:cols layout dtype CAD-KIND:address-space -- tensor ) TENSOR:TV-DESC" CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-NEG-LAYOUT-SPACE ( CAD-KIND:rows CAD-KIND:cols dtype CAD-KIND:address-space layout -- tensor ) TENSOR:TV-DESC" CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-NEG-RAW-HANDLE ( n -- tensor )" CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-NEG-HANDLE-RAW ( tensor -- n )" CHECK-QUIET-CANDIDATE! 0 T=

\ op-kind family negatives: a raw code cannot open a plan record, the stored
\ op cannot leak as n, and a dtype cannot cross into the op column (or back).
s" TVT-OP-OK    ( opkind -- ) TENSOR:PLAN-OP-BEGIN"                    CHECK-QUIET-CANDIDATE! -1 T=
s" TVT-OP-N     ( n -- ) TENSOR:PLAN-OP-BEGIN"                         CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-OP-DT    ( dtype -- ) TENSOR:PLAN-OP-BEGIN"                     CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-OP-NOUT  ( n -- n ) TENSOR:PLAN-OP@"                            CHECK-QUIET-CANDIDATE! 0 T=
s" TVT-OP-ASDT  ( n -- dtype ) TENSOR:PLAN-OP@"                        CHECK-QUIET-CANDIDATE! 0 T=

end-package

\ ---- forged handles fail closed (owner-package probes over the raw boundary) -
package TENSOR
private

: TVT-FORGED-IDX ( -- )
   TV-RESET
   1 1 MAKI:SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI:SPACE-GLOBAL TV-DESC drop
   TV-GEN @ TV-CAP * TV-U @ + RAW>TENSOR TV-ROWS@ drop ;

: TVT-FORGED-GEN ( -- )
   TV-RESET
   1 1 MAKI:SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MAKI:SPACE-GLOBAL TV-DESC drop
   TV-GEN @ 1+ TV-CAP * RAW>TENSOR TV-ROWS@ drop ;

' TVT-FORGED-IDX E-TV-HANDLE TTHROWS
' TVT-FORGED-GEN E-TV-HANDLE TTHROWS
TV-GEN-MAX TV-GEN !
' TV-RESET E-TV-GEN TTHROWS
0 TV-GEN ! 0 TV-U !

end-package

package MAKI

T-REPORT

end-package

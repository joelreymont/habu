\ maki/precision-test.f - checked tests for gate-licensed precision (maki/precision.f)
\ and its tolerance composition into the whole-model golden (maki/lower-golden.f).
\
\ Host-only: defaults (f32 everywhere), the f32 tolerance rows, a tf32 request for
\ the matmul class switching the selected rtol to 2e-3 (and ONLY for that class),
\ PREC-RESET restoring the default, every fail-closed throw (bad precision id, bad
\ class id, unlicensed pair), and the composed model tolerance (MDL-RTOL / MDL-ATOL /
\ MDL-PREC) picking up each region class's ACTIVE precision row. The device license
\ mechanism itself is proven on the Orin by maki/precision-device-test.f.

require lib/test.f
require lib/float.f
require maki/precision.f
require maki/cad.f
require maki/lower-golden.f

package MAKI

\ float equality within a hair (the rows are exact powers of ten times a mantissa)
: TR= ( r r -- )  f- fabs 0.000000000001 f< TTRUE ;

\ ---- fail-closed probes ------------------------------------------------------
: TRY-BAD-PREC   ( -- )  PREC-N CLASS-MATMUL PREC! ;
: TRY-NEG-PREC   ( -- )  -1 CLASS-MATMUL PREC! ;
: TRY-BAD-CLASS  ( -- )  PREC-F32 CLASS-N PREC! ;
: TRY-NEG-CLASS  ( -- )  PREC-F32 -1 PREC! ;
: TRY-TF32-EW    ( -- )  PREC-TF32 CLASS-EW PREC! ;          \ no tf32 row: unlicensed
: TRY-TF32-RED   ( -- )  PREC-TF32 CLASS-ROW-REDUCE PREC! ;
: TRY-F32-DECODE ( -- )  PREC-F32 CLASS-DECODE PREC! ;       \ decode has no golden row
: TRY-BAD-NAME   ( -- )  PREC-N PREC-NAME 2drop ;
: TRY-DECODE-TOL ( -- )  CLASS-DECODE PREC-RTOL fdrop ;      \ active row query fails closed

T-RESET

\ ---- names -------------------------------------------------------------------
PREC-F32  PREC-NAME s" f32"  T$=
PREC-TF32 PREC-NAME s" tf32" T$=

\ ---- defaults: f32 requested for every class ----------------------------------
PREC-RESET
CLASS-EW         PREC@ PREC-F32 T=
CLASS-ROW-REDUCE PREC@ PREC-F32 T=
CLASS-MATMUL     PREC@ PREC-F32 T=
CLASS-MOVEMENT   PREC@ PREC-F32 T=
CLASS-DECODE     PREC@ PREC-F32 T=

\ ---- f32 tolerance rows (the lower-golden slice 1-4 values) --------------------
CLASS-EW         PREC-RTOL 0.00001   TR=
CLASS-ROW-REDUCE PREC-RTOL 0.0001    TR=
CLASS-MATMUL     PREC-RTOL 0.0001    TR=
CLASS-MOVEMENT   PREC-RTOL 0.000001  TR=
CLASS-EW         PREC-ATOL 0.000001  TR=
CLASS-MATMUL     PREC-ATOL 0.000001  TR=

\ ---- licensed pairs ------------------------------------------------------------
CLASS-MATMUL PREC-F32  PREC-LICENSED? TTRUE
CLASS-MATMUL PREC-TF32 PREC-LICENSED? TTRUE
CLASS-EW     PREC-TF32 PREC-LICENSED? TFALSE
CLASS-DECODE PREC-F32  PREC-LICENSED? TFALSE

\ ---- a tf32 request switches ONLY the matmul class's selected row ---------------
PREC-TF32 CLASS-MATMUL PREC!
CLASS-MATMUL     PREC@ PREC-TF32 T=
CLASS-MATMUL     PREC-RTOL 0.002     TR=       \ tf32 row (measured ~8e-4, 2.5x headroom)
CLASS-MATMUL     PREC-ATOL 0.000001  TR=       \ atol stays at the f32 floor
CLASS-EW         PREC@ PREC-F32 T=             \ other classes untouched
CLASS-EW         PREC-RTOL 0.00001   TR=
CLASS-ROW-REDUCE PREC-RTOL 0.0001    TR=

\ ---- PREC-RESET restores the default -------------------------------------------
PREC-RESET
CLASS-MATMUL PREC@ PREC-F32 T=
CLASS-MATMUL PREC-RTOL 0.0001 TR=

\ ---- PREC-MAX (strongest demotion wins) ------------------------------------------
PREC-F32  PREC-F32  PREC-MAX PREC-F32  T=
PREC-F32  PREC-TF32 PREC-MAX PREC-TF32 T=
PREC-TF32 PREC-F32  PREC-MAX PREC-TF32 T=

\ ---- fail-closed throws ----------------------------------------------------------
' TRY-BAD-PREC   E-PREC-ID    TTHROWS      \ precision id out of range
' TRY-NEG-PREC   E-PREC-ID    TTHROWS
' TRY-BAD-CLASS  E-PREC-CLASS TTHROWS
' TRY-NEG-CLASS  E-PREC-CLASS TTHROWS
' TRY-TF32-EW    E-PREC-ROW   TTHROWS      \ unlicensed pair: no tf32 row for ew
' TRY-TF32-RED   E-PREC-ROW   TTHROWS
' TRY-F32-DECODE E-PREC-ROW   TTHROWS      \ decode carries no golden row at all
' TRY-BAD-NAME   E-PREC-ID    TTHROWS
' TRY-DECODE-TOL E-PREC-ROW   TTHROWS

\ ---- composed model tolerance uses each class's ACTIVE row -----------------------
\ FFN chain = 2 matmul regions + 1 row-reduce region (maki/lower-model-test.f facts).
MODEL: PFFN ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 -- y ) LINEAR GELU LINEAR RMSNORM ;
FP-BUILD
MDL-COUNT-REGIONS
MDL-N-MM@  2 T=
MDL-N-RED@ 1 T=
MDL-PREC PREC-F32 T=
MDL-RTOL 0.0003    TR=                          \ 2*1e-4 (mm) + 1e-4 (red)
MDL-ATOL 0.000003  TR=                          \ 3 regions * 1e-6

PREC-TF32 CLASS-MATMUL PREC!
MDL-PREC PREC-TF32 T=
MDL-RTOL 0.0041    TR=                          \ 2*2e-3 (tf32 mm) + 1e-4 (red)
MDL-ATOL 0.000003  TR=                          \ atol floor unchanged

PREC-RESET
MDL-PREC PREC-F32 T=
MDL-RTOL 0.0003 TR=

T-REPORT

;package

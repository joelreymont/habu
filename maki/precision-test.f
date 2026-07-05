\ maki/precision-test.f - checked tests for gate-licensed precision (maki/precision.f).
\
\ Host-only: defaults (f32 everywhere), the f32 tolerance rows, a tf32 request for
\ the matmul class switching the selected rtol to 2e-3 (and ONLY for that class),
\ PREC-RESET restoring the default, and every fail-closed throw (bad precision id,
\ bad class id, unlicensed pair). The device license mechanism itself is proven on
\ the Orin by maki/precision-device-test.f.

require lib/test.f
require lib/float.f
require maki/precision.f

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

PREC-RESET
T-REPORT

end-package

\ maki/from-scratch-model-test.f - the windowed MLP capture + committed seeded data.
\
\ Asserts the MODEL: capture (3 nodes, 8x2 output, f32/row), the deterministic
\ dataset (same seed -> exact same features and targets, independent of the
\ separately-seeded parameter stream), non-degenerate in-range features, and the
\ small non-zero symmetry-breaking parameter init. A couple of milli-unit values
\ pin the committed dataset as a regression lock.

require lib/test.f
require lib/float.f
require maki/from-scratch-model.f

package MAKI

\ round a float to signed milli-units (round half away from zero)
: SMT-MILLI ( r -- n )  1000.0 f*  dup f0< if 0.5 f- else 0.5 f+ then  f>s ;

variable SMT-X    \ stashed float (avoid top-level locals / deep juggling)
variable SMT-Y
variable SMT-W

T-RESET

\ ---- MODEL: capture (canonical line; re-issued so the IR is fresh here) ------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
MODEL-DEFINED? TTRUE
MIR-N@ 3 T=                                   \ LINEAR, GELU, LINEAR
MIR-IN-SLOTS@ 5 T=                            \ x, w1, b1, w2, b2
2 MIR-ROWS@ 8 T=  2 MIR-COLS@ 2 T=            \ output y = 8x2 (mu, logvar)
2 MIR-DT@ DTYPE>N DT-F32 T=  2 MIR-LAY@ LAYOUT>N LAY-ROW T=
0 MIR-ROWS@ 8 T=  0 MIR-COLS@ 16 T=           \ hidden = 8x16

\ ---- committed dataset: deterministic + in range ----------------------------
SC-GEN-DATA
SC-X 0 T-GET SMT-MILLI -84 T=                       \ regression lock (committed seed)
SC-Y 0 T-GET SMT-MILLI -623 T=
SC-Y 7 T-GET SMT-MILLI -192 T=

\ features are in [-1,1) and not all equal (non-degenerate)
SC-X 0 T-GET fabs 1.0 f< TTRUE
SC-X 1 T-GET fabs 1.0 f< TTRUE
SC-X 0 T-GET SC-X 1 T-GET f= TFALSE

\ same seed -> exact same data (stash, regenerate, compare bit-for-bit)
SC-X 0 T-GET SMT-X !
SC-Y 3 T-GET SMT-Y !
SC-GEN-DATA
SC-X 0 T-GET SMT-X @ f= TTRUE
SC-Y 3 T-GET SMT-Y @ f= TTRUE

\ ---- parameter init: deterministic, small, non-zero (symmetry broken) -------
SC-INIT-PARAMS
SC-W1 0 T-GET SMT-W !
SMT-W @ fabs 0.1 f< TTRUE                      \ |w| < 0.1
SMT-W @ 0.0 f= TFALSE                          \ not zero
SC-W2 0 T-GET 0.0 f= TFALSE
SC-INIT-PARAMS
SC-W1 0 T-GET SMT-W @ f= TTRUE                 \ same param seed -> same init

\ data stream is independent of the param stream: regenerate after param init
\ and the data still matches (SC-GEN-DATA reseeds to the data seed)
SC-Y 5 T-GET SMT-Y !
SC-GEN-DATA
SC-Y 5 T-GET SMT-Y @ f= TTRUE

T-REPORT

end-package

\ maki/from-scratch-test.f - the seeded convergence gate for the from-scratch flagship.
\
\ End-to-end, deterministic: gradcheck the model once (the same IR trains and
\ gradchecks), then train SCT-N steps from random init on the committed seeded
\ data and assert the batch Gaussian NLL falls - below the committed initial loss,
\ below initial*SC-CONV-RATIO, and below the SC-CONV-THRESH floor - so a planner
\ or numeric regression fails this gate like a wrong answer. Determinism is exact:
\ the whole pipeline is single-threaded float arithmetic in a fixed order, so the
\ same seed reproduces the final loss bit-for-bit (asserted with f=). Wired into
\ maki/test.f AFTER maki/from-scratch-train-test.f (which relies on SC-RUN not having
\ run yet for its fail-closed accessor tests).

require lib/test.f
require lib/float.f
require maki/from-scratch-train.f
require maki/gradcheck.f

package MAKI

60 constant SCT-N                              \ committed training steps (< 2s, converges)

variable SCT-L1                               \ stashed final loss (determinism check)

T-RESET

\ ---- gradcheck the model once before training (same IR trains + gradchecks) ---
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
GC-RUN V-PASS T=

\ ---- seeded end-to-end convergence ------------------------------------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
SCT-N SC-RUN
SC-STEPS@ SCT-N T=
SC-INITIAL@ SC-MILLI 130 T=                         \ committed initial batch NLL
SC-FINAL@   SC-MILLI -647 T=                         \ committed final NLL (regression lock)
SC-FINAL@ SC-INITIAL@ f< TTRUE                  \ loss strictly decreased
SC-FINAL@ SC-INITIAL@ SC-CONV-RATIO f* f< TTRUE \ below initial * ratio
SC-FINAL@ SC-CONV-THRESH f< TTRUE               \ below the committed NLL floor
SC-CONVERGED? TTRUE

\ ---- report renders the converged flag --------------------------------------
SCRATCH-REPORT s" converged=1" CONTAINS? TTRUE

\ ---- determinism: same seed -> identical final loss (exact) -----------------
SC-FINAL@ SCT-L1 !
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
SCT-N SC-RUN
SC-FINAL@ SCT-L1 @ f= TTRUE

T-REPORT

end-package

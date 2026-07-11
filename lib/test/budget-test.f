\ budget-test.f - focused coverage for load-aware budget scaling.
\ Run: bin/hb --load lib/test/budget-test.f

require lib/test.f

: TBT-PURE ( -- )                          \ factor math: measured/reference -> clamped pct
   95 95 T-BUDGET-CAL-PCT 100 T=
   190 95 T-BUDGET-CAL-PCT 200 T=
   950 95 T-BUDGET-CAL-PCT 300 T=          \ thrashing box clamps high
   40 95 T-BUDGET-CAL-PCT 100 T=           \ fast box clamps low
   95 0 T-BUDGET-CAL-PCT 100 T=            \ uncalibrated target -> nominal
   50 T-BUDGET-CLAMP 100 T=
   250 T-BUDGET-CLAMP 250 T=
   999 T-BUDGET-CLAMP 300 T= ;

: TBT-SELF ( -- )                          \ standalone self-calibration: live measure in band,
   T-BUDGET-SELF-PCT {: pct:n :}           \ and T-BUDGET-MS scales by exactly the cached factor
   pct T-BUDGET-MIN-PCT >= TTRUE
   pct T-BUDGET-MAX-PCT <= TTRUE
   T-BUDGET-PCT @ {: saved:n :}
   pct T-BUDGET-PCT !
   1000 T-BUDGET-MS pct 10 * T=
   saved T-BUDGET-PCT ! ;

T-RESET
TBT-PURE
TBT-SELF
T-REPORT

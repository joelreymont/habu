\ A leaving branch is re-encoded when a package object is installed elsewhere.
\ Exercise the public encoder through the real NBR package load.

require lib/test.f
require src/compiler/native/branch.f

package NBR-RELOC-TEST

public

: RUN ( -- )
   T-RESET
   s" B relocates both directions and refuses an unreachable target" T-LABEL
   4096 4104 NBR:B-WORD {: ahead:n :}
   ahead NBR:B? TTRUE
   4096 ahead NBR:B-TARGET 4104 T=
   4104 4096 NBR:B-WORD {: behind:n :}
   behind NBR:B? TTRUE
   4104 behind NBR:B-TARGET 4096 T=
   [: 0 134217728 NBR:B-WORD drop ;] E-NBR-RANGE TTHROWSQ
   T-REPORT ;

;package

NBR-RELOC-TEST:RUN

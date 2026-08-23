\ maki/golden.f - the host GOLDEN gate.
\
\ An external reference artifact is independent evidence, so GA-CHECK may produce
\ PASS or FAIL. Without one, the host executor has nothing independent to compare
\ against and GOLDEN is honestly NOT-RUN. The device-vs-host leg lives in cad.f.

require maki/report.f
require maki/golden-artifact.f

package MAKI
private

: GO-GATE-ARTIFACT ( report -- report )
   GA-CHECK {: v:n :}  GA-RE$ v REPORT:>VERDICT MAKI-GATE:GOLDEN REPORT:VERDICT!
   s" golden: external reference artifact comparison (per-artifact tolerance)" REPORT:WARN+ ;

: GO-GATE-MISSING ( report -- report )
   s" no independent golden evidence" MAKI-VERDICT:NOT-RUN MAKI-GATE:GOLDEN REPORT:VERDICT! ;

: HOST-INTO ( report -- report )
   GA-EXISTS? if GO-GATE-ARTIFACT else GO-GATE-MISSING then ;

;package

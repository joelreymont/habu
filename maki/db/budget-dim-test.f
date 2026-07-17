\ maki/db/budget-dim-test.f - checked acceptance for the budget-dimension vocabulary
\ (maki/db/budget-dim.f, dot habu-v2-capability-and-0970a96d).
\   BD-ROUND-*   : DIM>N and N>DIM are inverses over all six dimensions (stable wire ordinals)
\   BD-COUNT     : DIM-COUNT is the vector width (6)
\   BD-BAD       : an ordinal outside the closed 0..5 domain fails closed (E-BUDGET-DIM)

require lib/prelude.f
require lib/test.f
require maki/db/budget-dim.f

package BUDGET

: ROUND ( BUDGET:dim -- n )   DIM>N N>DIM DIM>N ;    \ dim -> ord -> dim -> ord (identity on the ordinal)
: BD-BAD ( -- )   6 N>DIM drop ;                     \ ordinal past the closed domain -> throw

T-RESET

BUDGET-DIM:COMPUTE-TIME     ROUND 0 T=
BUDGET-DIM:DEVICE-TIME      ROUND 1 T=
BUDGET-DIM:STORAGE          ROUND 2 T=
BUDGET-DIM:CANDIDATE-COUNT  ROUND 3 T=
BUDGET-DIM:RETRIES          ROUND 4 T=
BUDGET-DIM:EXTERNAL-EFFECTS ROUND 5 T=

DIM-COUNT 6 T=

' BD-BAD E-BUDGET-DIM TTHROWS

T-REPORT

;package

\ maki/eval-passk.f - exact unbiased pass@k estimator (package EVAL).
\
\ The eval matrix's pass@k column (docs/eval-triton.md): given n graded samples
\ of which c passed on the first attempt, the unbiased estimator is
\ pass@k = 1 - C(n-c,k)/C(n,k). Computed EXACTLY over integer falling
\ factorials (num = (n-c)_k, den = (n)_k) and reported per-mille
\ (x1000, the repo's fixed-point convention), rounded to nearest - the doc's
\ recorded n=5,c=3 round is 600 / 900 / 1000. No floats, so tallies replayed
\ from a committed transcript are bit-deterministic. Magnitude growth fails
\ closed (E-PASSK-CAP) instead of silently overflowing.

require lib/errors.f
require lib/string.f

\ eval-passk owns error codes -5260..-5261.
-5260 constant E-PASSK-ARG   \ n < 1, c outside 0..n, or k outside 1..n
-5261 constant E-PASSK-CAP   \ falling factorial would overflow the fixed point

package EVAL

variable PK-NUM   \ falling factorial (n-c)_k
variable PK-DEN   \ falling factorial (n)_k

: PK-GUARD ( n n n -- ) {: n:n c:n k:n :}
   n 1 < if E-PASSK-ARG throw then
   c 0 < c n > or if E-PASSK-ARG throw then
   k 1 < k n > or if E-PASSK-ARG throw then ;

\ one falling-factorial step; den*n must stay under MAX/1001 so the final
\ (den-num)*1000 + den/2 rounding sum cannot overflow
: PK-STEP ( n n n -- ) {: n:n c:n i:n :}
   PK-DEN @ STR-MAX-I64 1001 / n / > if E-PASSK-CAP throw then
   PK-NUM @ n c - i - *  PK-NUM !
   PK-DEN @ n i - *      PK-DEN ! ;

public

\ unbiased pass@k, per-mille: round(1000 * (1 - C(n-c,k)/C(n,k)))
: PASSK-X1000 ( n n n -- n ) {: n:n c:n k:n :}
   n c k PK-GUARD
   k n c - > if 1000 exit then          \ every k-draw contains a pass
   1 PK-NUM !  1 PK-DEN !
   k 0 ?do n c i PK-STEP loop
   PK-DEN @ PK-NUM @ - 1000 *  PK-DEN @ 2 / +  PK-DEN @ / ;

;package

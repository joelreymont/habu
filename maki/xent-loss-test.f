\ maki/xent-loss-test.f - stable log-softmax cross-entropy over logits + INTEGER
\ targets (LOSS:TT-XENT / LOSS:TT-XENT-SEED, maki/loss-tensor.f).
\
\ This suite lives in its own test-owned package (XENT-LOSS-TEST) and imports the
\ LOSS public surface with `using`, so no test helper leaks into a shared namespace.
\
\ Reference row (shared with maki/celoss-test.f): logits=[1,2,3], integer target class
\ 2. y=softmax=[0.0900,0.2447,0.6652]; L=-ln(y2)=0.40757; dL/dlogits=y-t=
\ [0.0900,0.2447,-0.3348]. Proves: forward matches the celoss golden; the y-t seed
\ matches the golden AND a central finite difference of EVERY logit (not just one);
\ shift-invariance keeps extreme logits finite; and every fail-closed guard fires -
\ non-finite / fractional / out-of-range targets (E-MK-TGT), bad dimensions
\ (E-MK-DIM), count mismatch (E-MK-SHAPE), the empty-batch policy, and no-partial-write.

require lib/test.f
require maki/loss-tensor.f

package XENT-LOSS-TEST
using LOSS

T-RESET

\ ---- reference row: logits + integer target class 2 -------------------------
create L 3 cells allot   create T 1 cells allot   create D 3 cells allot
1.0 L 0 T-SET   2.0 L 1 T-SET   3.0 L 2 T-SET
2.0 T 0 T-SET                                          \ integer class target = 2

\ forward: stable log-softmax CE == the celoss golden 0.40757
L 1 3 T 1 TT-XENT  1000.0 f* 0.5 f+ f>s  408 T=

\ seed cotangent dL/dlogits = y - onehot(target) (exact goldens)
L T D 1 3 1 TT-XENT-SEED
D 0 T-GET  1000.0 f* 0.5 f+ f>s   90 T=                \ y0 - 0
D 1 T-GET  1000.0 f* 0.5 f+ f>s  245 T=                \ y1 - 0
D 2 T-GET  1000.0 f* 0.5 f- f>s -335 T=                \ y2 - 1 = -0.3348

\ ---- central finite difference of EVERY logit == the y-t seed ---------------
\ The old suite differenced only logit 2; the seed is dL/dlogits for the WHOLE row,
\ so each logit's central difference must reproduce the matching seed component.
: CE ( -- r )  L 1 3 T 1 TT-XENT ;
: FD-LOGIT ( n -- r ) {: j:n :}
   L j T-GET {: b:r :}
   b 0.001 f+ L j T-SET  CE {: yp:r :}
   b 0.001 f- L j T-SET  CE {: ym:r :}
   b L j T-SET
   yp ym f-  0.002 f/ ;
0 FD-LOGIT  1000.0 f* 0.5 f+ f>s   90 T=
1 FD-LOGIT  1000.0 f* 0.5 f+ f>s  245 T=
2 FD-LOGIT  1000.0 f* 0.5 f- f>s -335 T=

\ ---- second exact y-t golden: uniform logits, target class 1 ----------------
\ y = [1/3,1/3,1/3]; L = ln 3 = 1.0986; y-t = [1/3, 1/3-1, 1/3].
create U 3 cells allot   create UD 3 cells allot
0.0 U 0 T-SET   0.0 U 1 T-SET   0.0 U 2 T-SET
1.0 T 0 T-SET                                          \ target class 1
U 1 3 T 1 TT-XENT  1000.0 f* 0.5 f+ f>s 1099 T=
U T UD 1 3 1 TT-XENT-SEED
UD 0 T-GET 1000.0 f* 0.5 f+ f>s  333 T=
UD 1 T-GET 1000.0 f* 0.5 f- f>s -667 T=
UD 2 T-GET 1000.0 f* 0.5 f+ f>s  333 T=
2.0 T 0 T-SET                                          \ restore reference target

\ ---- numerical stability: shift-invariance keeps extremes finite ------------
\ Logits ~+/-1000 overflow a naive exp; the row-max-shifted logsumexp stays finite
\ and (shift-invariant) equals the reference CE 0.40757 for the same [+0,+1,+2] gaps.
create HI 3 cells allot   create LO 3 cells allot
1000.0 HI 0 T-SET   1001.0 HI 1 T-SET   1002.0 HI 2 T-SET
HI 1 3 T 1 TT-XENT  1000.0 f* 0.5 f+ f>s  408 T=
-1000.0 LO 0 T-SET  -999.0 LO 1 T-SET  -998.0 LO 2 T-SET
LO 1 3 T 1 TT-XENT  1000.0 f* 0.5 f+ f>s  408 T=

\ ---- empty-batch policy: R=0 is zero loss and writes nothing ----------------
create Z 1 cells allot
Z 0 3 Z 0 TT-XENT  1000.0 f* 0.5 f+ f>s  0 T=          \ R=0 -> 0.0
: EMPTY-SEED-CANARY ( -- bool )  -7.0 Z 0 T-SET  Z Z Z 0 3 0 TT-XENT-SEED  Z 0 T-GET -7.0 f= ;
EMPTY-SEED-CANARY TTRUE                                \ R=0 seed leaves the buffer untouched

\ ---- multi-row: no-partial-write + invalid first / middle / last target -----
\ 3 rows x V=3. A rejected target anywhere must leave the whole seed buffer intact.
create ML 9 cells allot   create MT 3 cells allot   create MD 10 cells allot
1.0 ML 0 T-SET 2.0 ML 1 T-SET 3.0 ML 2 T-SET
1.0 ML 3 T-SET 2.0 ML 4 T-SET 3.0 ML 5 T-SET
1.0 ML 6 T-SET 2.0 ML 7 T-SET 3.0 ML 8 T-SET
: MD-FILL   ( -- )  10 0 ?do  -7.0 MD i T-SET  loop ;
: MD-INTACT? ( -- bool )  0  10 0 ?do  MD i T-GET -7.0 f= 0= if 1+ then  loop  0= ;
: MT-GOOD ( -- )  0.0 MT 0 T-SET  1.0 MT 1 T-SET  2.0 MT 2 T-SET ;
: SEED-ML  ( -- )  ML MT MD 3 3 3 TT-XENT-SEED ;
: BAD-FIRST  ( -- )  MT-GOOD  5.0 MT 0 T-SET  SEED-ML ;   \ row 0 target out of range
: BAD-MIDDLE ( -- )  MT-GOOD  5.0 MT 1 T-SET  SEED-ML ;   \ row 1 target out of range
: BAD-LAST   ( -- )  MT-GOOD  5.0 MT 2 T-SET  SEED-ML ;   \ row 2 target out of range
MD-FILL ' BAD-FIRST  E-MK-TGT TTHROWS   MD-INTACT? TTRUE
MD-FILL ' BAD-MIDDLE E-MK-TGT TTHROWS   MD-INTACT? TTRUE
MD-FILL ' BAD-LAST   E-MK-TGT TTHROWS   MD-INTACT? TTRUE
\ all-valid seed writes exactly R*V=9 cells; the canary at cell 9 is untouched
MT-GOOD  MD-FILL  SEED-ML
MD 9 T-GET -7.0 f= TTRUE                               \ no over-write past R*V
MD 0 T-GET 1000.0 f* 0.5 f- f>s -910 T=                \ row0 y0-1 = 0.0900-1
MD 4 T-GET 1000.0 f* 0.5 f- f>s -755 T=                \ row1 y1-1 = 0.2447-1
MD 8 T-GET 1000.0 f* 0.5 f- f>s -335 T=                \ row2 y2-1 = 0.6652-1

\ ---- target-value rejects: non-finite, fractional, and out of [0,V) ---------
\ Every non-conforming target throws E-MK-TGT and never becomes a plausible class id.
: BAD-FRAC ( -- )   2.5 T 0 T-SET  L 1 3 T 1 TT-XENT drop ;      \ 2.5 truncates to a valid id but is fractional
: BAD-NAN  ( -- )   0.0 0.0 f/ T 0 T-SET  L 1 3 T 1 TT-XENT drop ;
: BAD-PINF ( -- )   1.0 0.0 f/ T 0 T-SET  L 1 3 T 1 TT-XENT drop ;
: BAD-NINF ( -- )  -1.0 0.0 f/ T 0 T-SET  L 1 3 T 1 TT-XENT drop ;
: BAD-HI   ( -- )   3.0 T 0 T-SET  L 1 3 T 1 TT-XENT drop ;      \ 3 == V
: BAD-BIG  ( -- )   5.0 T 0 T-SET  L 1 3 T 1 TT-XENT drop ;      \ 5 > V
: BAD-NEG  ( -- )  -1.0 T 0 T-SET  L 1 3 T 1 TT-XENT drop ;
' BAD-FRAC E-MK-TGT TTHROWS
' BAD-NAN  E-MK-TGT TTHROWS
' BAD-PINF E-MK-TGT TTHROWS
' BAD-NINF E-MK-TGT TTHROWS
' BAD-HI   E-MK-TGT TTHROWS
' BAD-BIG  E-MK-TGT TTHROWS
' BAD-NEG  E-MK-TGT TTHROWS
2.0 T 0 T-SET                                          \ restore reference target

\ ---- dimension rejects: V<1, negative R, and an overflowing R*V --------------
: BAD-V0   ( -- )  L 1 0 T 1 TT-XENT drop ;            \ V=0
: BAD-VNEG ( -- )  L 1 -1 T 1 TT-XENT drop ;           \ V<0
: BAD-RNEG ( -- )  L -1 3 T -1 TT-XENT drop ;          \ R<0
: BAD-OVER ( -- )  L $4000000000000000 4 T 1 TT-XENT drop ;   \ R*V overflows a cell
' BAD-V0   E-MK-DIM TTHROWS
' BAD-VNEG E-MK-DIM TTHROWS
' BAD-RNEG E-MK-DIM TTHROWS
' BAD-OVER E-MK-DIM TTHROWS

\ ---- shape rejects: target count != row count (short and long) --------------
: BAD-SHORT ( -- )  ML MT MD 2 3 1 TT-XENT-SEED ;      \ tn=1 < R=2
: BAD-LONG  ( -- )  L 1 3 T 2 TT-XENT drop ;           \ tn=2 > R=1
' BAD-SHORT E-MK-SHAPE TTHROWS
' BAD-LONG  E-MK-SHAPE TTHROWS

T-REPORT

;using
;package

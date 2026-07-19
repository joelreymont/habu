\ maki/xent-loss-test.f - stable log-softmax cross-entropy over logits + INTEGER
\ targets (LOSS:TT-XENT / LOSS:TT-XENT-SEED, maki/loss-tensor.f).
\
\ Same reference row as maki/celoss-test.f: logits=[1,2,3], integer target=class 2.
\ y=softmax=[0.0900,0.2447,0.6652]; L=-ln(y2)=0.40757; dL/dlogits=y-t=[.0900,.2447,-.3348].
\ Proves: the forward matches the celoss golden; the y-t seed matches the golden and a
\ central finite difference; extreme logits that overflow a naive exp stay finite via
\ the stable logsumexp (shift-invariance); and both fail-closed rejects fire.

require lib/test.f
require maki/loss-tensor.f

package MAKI

T-RESET

create XCE-L 3 cells allot   create XCE-T 1 cells allot   create XCE-D 3 cells allot
1.0 XCE-L 0 T-SET   2.0 XCE-L 1 T-SET   3.0 XCE-L 2 T-SET
2.0 XCE-T 0 T-SET                                      \ integer class target = 2

\ ---- forward: stable log-softmax CE == the celoss golden -0.40757 ------------
XCE-L 1 3 XCE-T 1 LOSS:TT-XENT  1000.0 f* 0.5 f+ f>s  408 T=

\ ---- seed cotangent dL/dlogits = y - onehot(target) -------------------------
XCE-L XCE-T XCE-D 1 3 1 LOSS:TT-XENT-SEED
XCE-D 0 T-GET  1000.0 f* 0.5 f+ f>s    90 T=            \ y0 - 0
XCE-D 1 T-GET  1000.0 f* 0.5 f+ f>s   245 T=            \ y1 - 0
XCE-D 2 T-GET  1000.0 f* 0.5 f- f>s  -335 T=            \ y2 - 1 = -0.3348

\ ---- central finite difference dL/dlogits[2] == seed[2] = -0.3348 -----------
: XCE-CE ( -- r )  XCE-L 1 3 XCE-T 1 LOSS:TT-XENT ;
3.001 XCE-L 2 T-SET  XCE-CE
2.999 XCE-L 2 T-SET  XCE-CE
f-  0.002 f/  1000.0 f* 0.5 f- f>s  -335 T=
3.0 XCE-L 2 T-SET

\ ---- numerical stability: logits ~1000 overflow a naive exp; stable stays -----
\ finite and (shift-invariant) equals the CE of the small-logit row above (408).
create XCE-E 3 cells allot
1000.0 XCE-E 0 T-SET   1001.0 XCE-E 1 T-SET   1002.0 XCE-E 2 T-SET
XCE-E 1 3 XCE-T 1 LOSS:TT-XENT  1000.0 f* 0.5 f+ f>s  408 T=

\ ---- named rejects: target outside [0,V), and target-count != row-count ------
: XCE-BAD-TGT   ( -- )  5.0 XCE-T 0 T-SET  XCE-E 1 3 XCE-T 1 LOSS:TT-XENT f>s drop ;   \ 5 >= V=3
: XCE-BAD-SHAPE ( -- )  2.0 XCE-T 0 T-SET  XCE-E 1 3 XCE-T 2 LOSS:TT-XENT f>s drop ;   \ tn=2 != R=1
' XCE-BAD-TGT   E-MK-TGT   TTHROWS
' XCE-BAD-SHAPE E-MK-SHAPE TTHROWS

T-REPORT

;package

\ maki/xent-train-test.f - the cross-entropy classifier trainer end-to-end
\ (maki/xent-train.f).
\
\ Proves the y-t seed backprops correctly through BW-BUILD and drives training:
\ the exact IR that trains also gradchecks (analytic backward == central finite
\ difference, GC-RUN V-PASS); the committed initial batch mean CE; SGD reduces the
\ CE below initial, below initial*ratio, and below the committed floor; and the run
\ is bit-deterministic from a fresh capture. Data are the committed one-hot classes
\ (class c = r mod V) with integer targets read via f>s.

require lib/test.f
require lib/float.f
require maki/xent-train.f
require maki/gradcheck.f

package MAKI

60 constant XTT-N                              \ committed training steps

variable XTT-L1                               \ stashed final loss (determinism check)

T-RESET

\ ---- the exact IR that trains also gradchecks on host -----------------------
MODEL: XENT-MLP ( x:10x5 w:5x5 b:1x5 -- logits ) LINEAR ;
GC-RUN V-PASS T=

\ ---- seeded end-to-end convergence ------------------------------------------
MODEL: XENT-MLP ( x:10x5 w:5x5 b:1x5 -- logits ) LINEAR ;
XTT-N CET-RUN
CET-STEPS@ XTT-N T=
CET-INITIAL@ 1000.0 f* 0.5 f+ f>s 1606 T=            \ committed initial batch mean CE
CET-FINAL@   1000.0 f* 0.5 f+ f>s  188 T=            \ committed final mean CE (regression lock)
CET-FINAL@ CET-INITIAL@ f< TTRUE                     \ loss strictly decreased
CET-FINAL@ CET-INITIAL@ 0.5 f* f< TTRUE              \ below initial * 0.5
CET-FINAL@ 0.5 f< TTRUE                              \ below the committed CE floor

\ ---- determinism: same seed -> identical final loss (exact) -----------------
CET-FINAL@ XTT-L1 !
MODEL: XENT-MLP ( x:10x5 w:5x5 b:1x5 -- logits ) LINEAR ;
XTT-N CET-RUN
CET-FINAL@ XTT-L1 @ f= TTRUE

T-REPORT

;package

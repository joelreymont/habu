\ maki/loss-test.f - runnable tests for the maki losses.

require lib/test.f
require maki/loss.f

package MAKI

T-RESET

\ MSE: pred=0.5 tgt=0.0 -> 0.25 ; x4 -> 1
0.5 0.0 MSE        4.0 f* 0.5 f+ f>s  1 T=
\ MSE: pred=1.0 tgt=0.5 -> 0.25 ; x4 -> 1
1.0 0.5 MSE        4.0 f* 0.5 f+ f>s  1 T=

\ MSE-GRAD: pred=0.5 tgt=0.0 -> 1.0 ; x4 -> 4
0.5 0.0 MSE-GRAD   4.0 f* 0.5 f+ f>s  4 T=
\ MSE-GRAD: pred=0.0 tgt=0.5 -> -1.0 ; +2.0 -> 1.0 ; x4 -> 4
0.0 0.5 MSE-GRAD   2.0 f+ 4.0 f* 0.5 f+ f>s  4 T=

\ L1: pred=0.5 tgt=1.0 -> 0.5 ; x4 -> 2
0.5 1.0 L1         4.0 f* 0.5 f+ f>s  2 T=

\ --- Gaussian NLL (log-variance parameterization). All tests x1000 + round. ---
\ y=1 mu=0.5 lv=0 -> prec=1, res2=0.25 -> NLL=0.5*(0+0.25)=0.125
1.0 0.5 0.0 NLL              1000.0 f* 0.5 f+ f>s   125 T=
\ FEXP path: y=2 mu=0 lv=ln2 -> prec=0.5, res2=4 -> 0.5*(ln2+2)=1.3466
2.0 0.0 0.6931471805599453 NLL  1000.0 f* 0.5 f+ f>s  1347 T=
\ dNLL/dmu = (mu-y)*prec = (0.5-1)*1 = -0.5
1.0 0.5 0.0 NLL-MU-GRAD      1000.0 f* 0.5 f- f>s  -500 T=
\ dNLL/dlogvar = 0.5*(1-res2*prec) = 0.5*(1-0.25) = 0.375
1.0 0.5 0.0 NLL-LOGVAR-GRAD  1000.0 f* 0.5 f+ f>s   375 T=
\ central FD dNLL/dmu at (1,0.5,0) = -0.5
1.0 0.501 0.0 NLL  1.0 0.499 0.0 NLL  f-  0.002 f/  1000.0 f* 0.5 f- f>s  -500 T=
\ central FD dNLL/dlogvar at (1,0.5,0) = 0.375
1.0 0.5 0.001 NLL  1.0 0.5 -0.001 NLL  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  375 T=

\ --- Mahalanobis (diagonal covariance v1). ---
\ y=3 mu=1 var=4 -> (3-1)^2/4 = 1.0
3.0 1.0 4.0 MAHALANOBIS        1000.0 f* 0.5 f+ f>s  1000 T=
\ dMah/dmu = 2*(mu-y)/var = 2*(1-3)/4 = -1.0
3.0 1.0 4.0 MAHALANOBIS-GRAD   1000.0 f* 0.5 f- f>s  -1000 T=
\ central FD dMah/dmu at (3,1,4) = -1.0
3.0 1.001 4.0 MAHALANOBIS  3.0 0.999 4.0 MAHALANOBIS  f-  0.002 f/  1000.0 f* 0.5 f- f>s  -1000 T=
\ VAR-OK passes a valid variance straight through
4.0 VAR-OK                    1000.0 f* 0.5 f+ f>s  4000 T=

\ Mahalanobis fails closed on non-positive variance (never clamps).
: BAD-VAR-ZERO ( -- )  3.0 1.0 0.0  MAHALANOBIS       f>s drop ;
: BAD-VAR-NEG  ( -- )  3.0 1.0 -2.0 MAHALANOBIS       f>s drop ;
: BAD-VAR-GRAD ( -- )  3.0 1.0 0.0  MAHALANOBIS-GRAD  f>s drop ;
: BAD-VAR-OK   ( -- )  0.0 VAR-OK                     f>s drop ;
' BAD-VAR-ZERO E-MK-VAR TTHROWS
' BAD-VAR-NEG  E-MK-VAR TTHROWS
' BAD-VAR-GRAD E-MK-VAR TTHROWS
' BAD-VAR-OK   E-MK-VAR TTHROWS

\ --- Huber (named transition delta). HUBER-DELTA = 1.0. ---
\ quadratic region |r|=0.5<=1 -> 0.5*0.5^2 = 0.125
0.5 0.0 HUBER-DELTA HUBER          1000.0 f* 0.5 f+ f>s   125 T=
\ quadratic grad = r = 0.5
0.5 0.0 HUBER-DELTA HUBER-GRAD     1000.0 f* 0.5 f+ f>s   500 T=
\ linear region |r|=3>1 -> 1*(3-0.5) = 2.5
3.0 0.0 HUBER-DELTA HUBER          1000.0 f* 0.5 f+ f>s  2500 T=
\ linear grad = delta*sign(r) = +1
3.0 0.0 HUBER-DELTA HUBER-GRAD     1000.0 f* 0.5 f+ f>s  1000 T=
\ linear region, negative residual -> loss 2.5, grad = -1
-3.0 0.0 HUBER-DELTA HUBER         1000.0 f* 0.5 f+ f>s  2500 T=
-3.0 0.0 HUBER-DELTA HUBER-GRAD    1000.0 f* 0.5 f- f>s -1000 T=
\ central FD grad in quadratic region at pred=0.5 -> 0.5
0.501 0.0 HUBER-DELTA HUBER  0.499 0.0 HUBER-DELTA HUBER  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  500 T=
\ central FD grad in linear region at pred=3.0 -> 1.0
3.001 0.0 HUBER-DELTA HUBER  2.999 0.0 HUBER-DELTA HUBER  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  1000 T=

T-REPORT

end-package

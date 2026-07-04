\ maki/loss-tensor-test.f - gradcheck tensor MSE loss + its gradient.
\ pred=[3,5], tgt=[1,1] -> L=(3-1)^2+(5-1)^2=20 ; dL/dpred=[4,8].

require lib/test.f
require maki/loss-tensor.f

package MAKI

T-RESET

create LP 2 cells allot   create LT 2 cells allot   create LTDY 2 cells allot
3.0 LP 0 T-SET   5.0 LP 1 T-SET
1.0 LT 0 T-SET   1.0 LT 1 T-SET

LP LT 2 TT-MSE-LOSS  0.5 f+ f>s  20 T=        \ L = 4 + 16
LP LT LTDY 2 TT-MSE-DY
LTDY 0 T-GET  0.5 f+ f>s  4 T=                 \ 2*(3-1)
LTDY 1 T-GET  0.5 f+ f>s  8 T=                 \ 2*(5-1)

\ central finite difference dL/dpred[0] = 4
3.001 LP 0 T-SET  LP LT 2 TT-MSE-LOSS
2.999 LP 0 T-SET  LP LT 2 TT-MSE-LOSS
f-  0.002 f/  0.5 f+ f>s  4 T=
3.0 LP 0 T-SET

\ --- Gaussian NLL over a 2-element tensor. All checks x1000 + round. ---
\ y=[1,2] mu=[0.5,0] lv=[0,ln2] -> NLL=[0.125, 0.5*(ln2+2)=1.3466] ; sum=1.4716
create LNY 2 cells allot   create LNM 2 cells allot   create LNLV 2 cells allot
create LNDMU 2 cells allot  create LNDLV 2 cells allot
1.0 LNY 0 T-SET   2.0 LNY 1 T-SET
0.5 LNM 0 T-SET   0.0 LNM 1 T-SET
0.0 LNLV 0 T-SET  0.6931471805599453 LNLV 1 T-SET

LNY LNM LNLV 2 TT-NLL-LOSS         1000.0 f* 0.5 f+ f>s  1472 T=
LNY LNM LNLV LNDMU 2 TT-NLL-DMU
LNDMU 0 T-GET  1000.0 f* 0.5 f- f>s  -500 T=       \ (0.5-1)*1
LNDMU 1 T-GET  1000.0 f* 0.5 f- f>s -1000 T=       \ (0-2)*0.5
LNY LNM LNLV LNDLV 2 TT-NLL-DLV
LNDLV 0 T-GET  1000.0 f* 0.5 f+ f>s   375 T=        \ 0.5*(1-0.25)
LNDLV 1 T-GET  1000.0 f* 0.5 f- f>s  -500 T=        \ 0.5*(1-2)
\ central FD dL/dmu[0] over the summed NLL = -0.5
0.501 LNM 0 T-SET  LNY LNM LNLV 2 TT-NLL-LOSS
0.499 LNM 0 T-SET  LNY LNM LNLV 2 TT-NLL-LOSS
f-  0.002 f/  1000.0 f* 0.5 f- f>s  -500 T=
0.5 LNM 0 T-SET

\ --- Mahalanobis (diagonal covariance) over a 2-element tensor. ---
\ y=[3,5] mu=[1,1] var=[4,2] -> [(2^2)/4, (4^2)/2] = [1, 8] ; sum=9
create LMY 2 cells allot   create LMM 2 cells allot   create LMV 2 cells allot
create LMDMU 2 cells allot  create LMVBAD 2 cells allot
3.0 LMY 0 T-SET   5.0 LMY 1 T-SET
1.0 LMM 0 T-SET   1.0 LMM 1 T-SET
4.0 LMV 0 T-SET   2.0 LMV 1 T-SET

LMY LMM LMV 2 TT-MAHALANOBIS-LOSS  1000.0 f* 0.5 f+ f>s  9000 T=
LMY LMM LMV LMDMU 2 TT-MAHALANOBIS-DMU
LMDMU 0 T-GET  1000.0 f* 0.5 f- f>s -1000 T=       \ 2*(1-3)/4
LMDMU 1 T-GET  1000.0 f* 0.5 f- f>s -4000 T=       \ 2*(1-5)/2

\ degenerate variance in the buffer fails the whole reduction closed (E-MK-VAR).
4.0 LMVBAD 0 T-SET   0.0 LMVBAD 1 T-SET
: BAD-TT-VAR     ( -- )  LMY LMM LMVBAD 2 TT-MAHALANOBIS-LOSS f>s drop ;
: BAD-TT-VAR-DMU ( -- )  LMY LMM LMVBAD LMDMU 2 TT-MAHALANOBIS-DMU ;
' BAD-TT-VAR     E-MK-VAR TTHROWS
' BAD-TT-VAR-DMU E-MK-VAR TTHROWS

\ --- Huber over a 2-element tensor. delta=HUBER-DELTA=1.0. ---
\ pred=[0.5,3] tgt=[0,0] -> [0.5*0.25, 1*(3-0.5)] = [0.125, 2.5] ; sum=2.625
create LHP 2 cells allot   create LHT 2 cells allot   create LHDY 2 cells allot
0.5 LHP 0 T-SET   3.0 LHP 1 T-SET
0.0 LHT 0 T-SET   0.0 LHT 1 T-SET

LHP LHT HUBER-DELTA 2 TT-HUBER-LOSS  1000.0 f* 0.5 f+ f>s  2625 T=
LHP LHT HUBER-DELTA LHDY 2 TT-HUBER-DY
LHDY 0 T-GET  1000.0 f* 0.5 f+ f>s   500 T=         \ quadratic grad = r
LHDY 1 T-GET  1000.0 f* 0.5 f+ f>s  1000 T=         \ linear grad = +delta
\ central FD dL/dpred[1] (linear region) = 1.0
3.001 LHP 1 T-SET  LHP LHT HUBER-DELTA 2 TT-HUBER-LOSS
2.999 LHP 1 T-SET  LHP LHT HUBER-DELTA 2 TT-HUBER-LOSS
f-  0.002 f/  1000.0 f* 0.5 f+ f>s  1000 T=
3.0 LHP 1 T-SET

T-REPORT

end-package

\ maki/attention-test.f - numerically verify single-head attention forward.
\
\ Q=K=I_2 (2x2), V=[[1,2],[3,4]]. S=Q.K^T=I, /sqrt(2)=[[0.707,0],[0,0.707]].
\ softmax rows -> A=[[0.6698,0.3302],[0.3302,0.6698]]. O=A.V=[[1.660,2.660],[2.340,3.340]].

require lib/test.f
require maki/attention.f

package MAKI

T-RESET

create AQ 4 cells allot   create AK 4 cells allot   create AV 4 cells allot
create AS 4 cells allot   create AA 4 cells allot   create AO 4 cells allot

1.0 AQ 0 T-SET  0.0 AQ 1 T-SET  0.0 AQ 2 T-SET  1.0 AQ 3 T-SET   \ Q = I
1.0 AK 0 T-SET  0.0 AK 1 T-SET  0.0 AK 2 T-SET  1.0 AK 3 T-SET   \ K = I
1.0 AV 0 T-SET  2.0 AV 1 T-SET  3.0 AV 2 T-SET  4.0 AV 3 T-SET   \ V

AQ AK AV AS AA AO  2 2  ATTN-FWD

AO 0 T-GET  1000.0 f* 0.5 f+ f>s  1660 T=        \ O[0,0]
AO 1 T-GET  1000.0 f* 0.5 f+ f>s  2660 T=        \ O[0,1]
AO 2 T-GET  1000.0 f* 0.5 f+ f>s  2340 T=        \ O[1,0]
AO 3 T-GET  1000.0 f* 0.5 f+ f>s  3340 T=        \ O[1,1]

\ sanity: each attention row sums to 1
AA 0 T-GET  AA 1 T-GET  f+  1000.0 f* 0.5 f+ f>s  1000 T=
AA 2 T-GET  AA 3 T-GET  f+  1000.0 f* 0.5 f+ f>s  1000 T=

\ ---- backward gradcheck: L = O[0,0] (dO = [[1,0],[0,0]]) ; analytic vs FD ----
create ATDO 4 cells allot  create ATDV 4 cells allot  create ATDA 4 cells allot
create ATDS 4 cells allot  create ATDQ 4 cells allot  create ATDK 4 cells allot
1.0 ATDO 0 T-SET  0.0 ATDO 1 T-SET  0.0 ATDO 2 T-SET  0.0 ATDO 3 T-SET

: AFWD0 ( -- r )  AQ AK AV AS AA AO 2 2 ATTN-FWD  AO 0 T-GET ;

AQ AK AV AS AA AO  2 2  ATTN-FWD                 \ refresh the saved attention AA
ATDO AQ AK AV AA  ATDV ATDA ATDS ATDQ ATDK  2 2  ATTN-BWD

\ dV[0,0] (= A[0,0]) : analytic vs central FD on V[0,0]
ATDV 0 T-GET  1000.0 f* 0.5 f+ f>s
   1.001 AV 0 T-SET AFWD0  0.999 AV 0 T-SET AFWD0  f- 0.002 f/  1.0 AV 0 T-SET
   1000.0 f* 0.5 f+ f>s  T=
\ dQ[0,0] : analytic vs FD on Q[0,0]  (through softmax)
ATDQ 0 T-GET  1000.0 f* 0.5 f+ f>s
   1.001 AQ 0 T-SET AFWD0  0.999 AQ 0 T-SET AFWD0  f- 0.002 f/  1.0 AQ 0 T-SET
   1000.0 f* 0.5 f+ f>s  T=
\ dK[0,1] : analytic vs FD on K[0,1] (index 1, orig 0.0)
ATDK 1 T-GET  1000.0 f* 0.5 f+ f>s
   0.001 AK 1 T-SET AFWD0  -0.001 AK 1 T-SET AFWD0  f- 0.002 f/  0.0 AK 1 T-SET
   1000.0 f* 0.5 f+ f>s  T=

T-REPORT

;package

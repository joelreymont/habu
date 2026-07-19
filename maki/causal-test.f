\ maki/causal-test.f - causal-masked softmax (maki/causal.f): forward golden over
\ MM-NT scores, adjoint gradcheck vs central finite difference, the perturb-future
\ invariance that autoregressive attention depends on, and the unmasked degeneracy.
\
\ Forward golden (L=3, d=2). Q=K rows (1,0),(0,1),(1,1); S[i,j]=Q_i.K_j:
\   row0 [1,0,1]  row1 [0,1,1]  row2 [1,1,2].
\ Causal softmax (row i over cols 0..i; e^0=1, e^1=2.71828, e^2=7.38906):
\   row0 valid{0}     [1]     -> [1,        0,       0]
\   row1 valid{0,1}   [0,1]   -> [0.26894,  0.73106, 0]        (Z=3.71828)
\   row2 valid{0,1,2} [1,1,2] -> [0.21194,  0.21194, 0.57612]  (Z=12.82562)
\ Names are MSK-prefixed: the maki suite loads every -test.f into one shared
\ dictionary, so top-level create/: names must be globally unique.

require lib/test.f
require lib/prelude.f
require maki/attention.f
require maki/causal.f

package MAKI

T-RESET

\ exact bit-for-bit element compare (perturb-invariance + unmasked degeneracy)
: MSK-EQ? ( ptr a ptr a n -- bool ) {: a:ptr b:ptr n:n :}
   n 0 ?do  a i T-GET b i T-GET f= 0= if false unloop exit then  loop  true ;

\ ---- forward golden: causal-masked softmax of MM-NT scores ------------------
create MSKQ 6 cells allot   create MSKK 6 cells allot
create MSKS 9 cells allot   create MSKA 9 cells allot
1.0 MSKQ 0 T-SET  0.0 MSKQ 1 T-SET   0.0 MSKQ 2 T-SET  1.0 MSKQ 3 T-SET   1.0 MSKQ 4 T-SET  1.0 MSKQ 5 T-SET
1.0 MSKK 0 T-SET  0.0 MSKK 1 T-SET   0.0 MSKK 2 T-SET  1.0 MSKK 3 T-SET   1.0 MSKK 4 T-SET  1.0 MSKK 5 T-SET

MSKQ MSKK MSKS  3 3 2  MM-NT
MSKS MSKA 3 CAUSAL-SOFTMAX-ROWS

MSKA 0 T-GET 1000.0 f* FROUND  1000 T=          \ row0: attends to itself only
MSKA 1 T-GET 1000.0 f* FROUND     0 T=          \ masked (future)
MSKA 2 T-GET 1000.0 f* FROUND     0 T=          \ masked (future)
MSKA 3 T-GET 1000.0 f* FROUND   269 T=          \ row1: softmax[0,1]
MSKA 4 T-GET 1000.0 f* FROUND   731 T=
MSKA 5 T-GET 1000.0 f* FROUND     0 T=          \ masked (future)
MSKA 6 T-GET 1000.0 f* FROUND   212 T=          \ row2: softmax[1,1,2]
MSKA 7 T-GET 1000.0 f* FROUND   212 T=
MSKA 8 T-GET 1000.0 f* FROUND   576 T=

\ each unmasked row is a distribution (sums to 1)
MSKA 0 T-GET                                     1000.0 f* FROUND  1000 T=
MSKA 3 T-GET MSKA 4 T-GET f+                     1000.0 f* FROUND  1000 T=
MSKA 6 T-GET MSKA 7 T-GET f+ MSKA 8 T-GET f+     1000.0 f* FROUND  1000 T=

\ ---- perturb-future-invariance: scores at j>i must not move ANY output -------
\ Recompute after adding arbitrary deltas to the strict-upper (future) entries
\ S[0,1],S[0,2],S[1,2]; every output element must be bit-identical to MSKA.
create MSKA2 9 cells allot
 5.0 MSKS 1 T-GET f+  MSKS 1 T-SET
-3.0 MSKS 2 T-GET f+  MSKS 2 T-SET
 9.0 MSKS 5 T-GET f+  MSKS 5 T-SET
MSKS MSKA2 3 CAUSAL-SOFTMAX-ROWS
MSKA MSKA2 9 MSK-EQ? TTRUE

\ ---- adjoint gradcheck: single row n=4, v=3 (col 3 masked) -------------------
\ x=[0.5,1,1.5,2], seed=[1,0.3,0.7,0.9]. y over [0,3)=[0.18632,0.30725,0.50643], y3=0.
\ d = sum_{j<3} seed_j y_j = 0.63300. dx_j = y_j (seed_j - d):
\   dx0=0.06838  dx1=-0.10231  dx2=0.03393  dx3=0 (masked).
\ Analytic (SM-BWD-CAUSAL) checked against central FD of L = sum_k seed_k y_k.
create MSKX 4 cells allot   create MSKY 4 cells allot   create MSKSEED 4 cells allot
create MSKDY 4 cells allot  create MSKDX 4 cells allot
0.5 MSKX 0 T-SET  1.0 MSKX 1 T-SET  1.5 MSKX 2 T-SET  2.0 MSKX 3 T-SET
1.0 MSKSEED 0 T-SET  0.3 MSKSEED 1 T-SET  0.7 MSKSEED 2 T-SET  0.9 MSKSEED 3 T-SET
1.0 MSKDY 0 T-SET  0.3 MSKDY 1 T-SET  0.7 MSKDY 2 T-SET  0.9 MSKDY 3 T-SET

: MSK-LOSS ( -- r )
   MSKX MSKY 4 3 SM-FWD-CAUSAL
   0.0  4 0 ?do  MSKSEED i T-GET  MSKY i T-GET  f*  f+  loop ;

MSKX MSKY 4 3 SM-FWD-CAUSAL
MSKDY MSKY MSKDX 4 3 SM-BWD-CAUSAL

MSKDX 0 T-GET 1000.0 f* FROUND
   0.501 MSKX 0 T-SET MSK-LOSS  0.499 MSKX 0 T-SET MSK-LOSS  f- 0.002 f/  0.5 MSKX 0 T-SET
   1000.0 f* FROUND  T=
MSKDX 1 T-GET 1000.0 f* FROUND
   1.001 MSKX 1 T-SET MSK-LOSS  0.999 MSKX 1 T-SET MSK-LOSS  f- 0.002 f/  1.0 MSKX 1 T-SET
   1000.0 f* FROUND  T=
MSKDX 2 T-GET 1000.0 f* FROUND
   1.501 MSKX 2 T-SET MSK-LOSS  1.499 MSKX 2 T-SET MSK-LOSS  f- 0.002 f/  1.5 MSKX 2 T-SET
   1000.0 f* FROUND  T=
\ masked input: perturbing x3 cannot move the loss, so both analytic and FD are 0
MSKDX 3 T-GET 1000.0 f* FROUND
   2.001 MSKX 3 T-SET MSK-LOSS  1.999 MSKX 3 T-SET MSK-LOSS  f- 0.002 f/  2.0 MSKX 3 T-SET
   1000.0 f* FROUND  T=

\ ---- unmasked degeneracy: v=n reproduces plain SM-FWD bit-for-bit ------------
create MSKEX 3 cells allot  create MSKYC 3 cells allot  create MSKYP 3 cells allot
1.0 MSKEX 0 T-SET  2.0 MSKEX 1 T-SET  3.0 MSKEX 2 T-SET
MSKEX MSKYC 3 3 SM-FWD-CAUSAL
MSKEX MSKYP 3 SM-FWD
MSKYC MSKYP 3 MSK-EQ? TTRUE

\ ---- a fully-masked row is impossible: v<1 and v>n both reject ---------------
: MSK-V0    ( -- )  MSKX MSKY 4 0 SM-FWD-CAUSAL ;
: MSK-VOVER ( -- )  MSKX MSKY 4 5 SM-FWD-CAUSAL ;
: MSK-THROWS ( -- )
   [: MSK-V0    ;] E-CAUSAL-RANGE TTHROWSQ
   [: MSK-VOVER ;] E-CAUSAL-RANGE TTHROWSQ ;
MSK-THROWS

T-REPORT

;package

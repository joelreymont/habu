\ maki/adamw-test.f - AdamW (decoupled weight decay) golden + param-group policy.
\
\ Proves, on fixed tensors + a fixed gradient sequence (no model run):
\  (a) DISTINGUISHING golden - AdamW with wd>0 follows a hand-computed decoupled
\      trajectory AND differs from coupled L2 (wd folded into the gradient). With
\      g=1 fixed, m/v start 0, lr=0.1, b1=0.9, b2=0.999, eps=0, wd=0.5:
\        decoupled  w_{t+1} = w_t - lr*wd*w_t - lr = 0.95*w_t - 0.1
\                   -> w1=0.85, w2=0.7075 ; m1=0.1 (RAW-gradient moment)
\        coupled L2 g'=g+wd*w=1.5 -> m1=0.15, w1=0.9  (moments see wd, so w differs)
\  (b) NO REGRESSION - TT-ADAMW! with wd=0 is bit-identical to TT-ADAM!.
\  (c) PARAM-GROUP POLICY - the trainer's checked decay attribute (ADAMW-WD-FOR)
\      routes wd only to WD-DECAY params; a WD-NONE param is bit-identical to
\      plain Adam (untouched by wd), a WD-DECAY param is not.
\  Red guards: negative wd -> E-ADAMW-WD; a flag outside {WD-DECAY,WD-NONE} ->
\  E-ADAMW-POLICY.
\ maki -> habu only.

require lib/test.f
require lib/float.f
require maki/adam-train.f

package MAKI

\ ================= (a) distinguishing golden: decoupled vs coupled =============
create AW-W 1 cells allot   create AW-G 1 cells allot
create AW-M 1 cells allot   create AW-V 1 cells allot
create L2-W 1 cells allot   create L2-G 1 cells allot
create L2-M 1 cells allot   create L2-V 1 cells allot

T-RESET

\ decoupled AdamW, step 1: w=1 g=1 m=v=0, lr=.1 b1=.9 b2=.999 eps=0 c1=.1 c2=.001 wd=.5
1.0 AW-W 0 T-SET   1.0 AW-G 0 T-SET   0.0 AW-M 0 T-SET   0.0 AW-V 0 T-SET
0.1 0.9 0.999 0.0 0.1 0.001 0.5 AW-W AW-G AW-M AW-V 1 OPTIM:TT-ADAMW!
AW-W 0 T-GET  10000.0 f* 0.5 f+ f>s  8500 T=      \ w1 = 0.85
AW-M 0 T-GET  10000.0 f* 0.5 f+ f>s  1000 T=      \ m1 = 0.10 (raw-gradient moment)
AW-V 0 T-GET  1000.0 f* 0.5 f+ f>s   1 T=         \ v1 = 0.001

\ step 2: continue with g=1, t=2 -> c1=1-.9^2=.19, c2=1-.999^2=.001999
0.1 0.9 0.999 0.0 0.19 0.001999 0.5 AW-W AW-G AW-M AW-V 1 OPTIM:TT-ADAMW!
AW-W 0 T-GET  10000.0 f* 0.5 f+ f>s  7075 T=      \ w2 = 0.7075

\ coupled L2, step 1 from the SAME start: g' = g + wd*w = 1.5, then plain Adam
1.0 L2-W 0 T-SET   0.0 L2-M 0 T-SET   0.0 L2-V 0 T-SET
1.0 1.0 0.5 OPTIM:WEIGHT-DECAY  L2-G 0 T-SET      \ g' = g + wd*w
0.1 0.9 0.999 0.0 0.1 0.001 L2-W L2-G L2-M L2-V 1 OPTIM:TT-ADAM!
L2-W 0 T-GET  10000.0 f* 0.5 f+ f>s  9000 T=      \ coupled w1 = 0.90
L2-M 0 T-GET  10000.0 f* 0.5 f+ f>s  1500 T=      \ coupled m1 = 0.15 (wd in moment)

\ the two rules are genuinely different objects (not just "decay happened"):
\ reset AW to its post-step-1 value and compare against coupled step 1.
1.0 AW-W 0 T-SET   1.0 AW-G 0 T-SET   0.0 AW-M 0 T-SET   0.0 AW-V 0 T-SET
0.1 0.9 0.999 0.0 0.1 0.001 0.5 AW-W AW-G AW-M AW-V 1 OPTIM:TT-ADAMW!
AW-W 0 T-GET  L2-W 0 T-GET  f=  TFALSE            \ decoupled w != coupled w
AW-M 0 T-GET  L2-M 0 T-GET  f=  TFALSE            \ decoupled m != coupled m

\ ================= (b) wd=0 is bit-identical to TT-ADAM! =======================
create ID-W 3 cells allot   create ID-M 3 cells allot   create ID-V 3 cells allot
create RF-W 3 cells allot   create RF-M 3 cells allot   create RF-V 3 cells allot
create Z-G  3 cells allot                          \ shared gradient (read-only)

1.0 ID-W 0 T-SET   2.0 ID-W 1 T-SET   0.0 0.5 f- ID-W 2 T-SET
1.0 RF-W 0 T-SET   2.0 RF-W 1 T-SET   0.0 0.5 f- RF-W 2 T-SET
1.0 Z-G  0 T-SET   0.0 2.0 f- Z-G  1 T-SET   0.5 Z-G  2 T-SET
0.1 ID-M 0 T-SET   0.2 ID-M 1 T-SET   0.0 ID-M 2 T-SET
0.1 RF-M 0 T-SET   0.2 RF-M 1 T-SET   0.0 RF-M 2 T-SET
0.001 ID-V 0 T-SET   0.004 ID-V 1 T-SET   0.002 ID-V 2 T-SET
0.001 RF-V 0 T-SET   0.004 RF-V 1 T-SET   0.002 RF-V 2 T-SET

0.1 0.9 0.999 0.0 0.1 0.001 0.0 ID-W Z-G ID-M ID-V 3 OPTIM:TT-ADAMW!
0.1 0.9 0.999 0.0 0.1 0.001     RF-W Z-G RF-M RF-V 3 OPTIM:TT-ADAM!
ID-W 0 T-GET RF-W 0 T-GET f= TTRUE   ID-M 0 T-GET RF-M 0 T-GET f= TTRUE   ID-V 0 T-GET RF-V 0 T-GET f= TTRUE
ID-W 1 T-GET RF-W 1 T-GET f= TTRUE   ID-M 1 T-GET RF-M 1 T-GET f= TTRUE   ID-V 1 T-GET RF-V 1 T-GET f= TTRUE
ID-W 2 T-GET RF-W 2 T-GET f= TTRUE   ID-M 2 T-GET RF-M 2 T-GET f= TTRUE   ID-V 2 T-GET RF-V 2 T-GET f= TTRUE

\ ================= (c) param-group policy: WD-NONE untouched, WD-DECAY not =====
\ direct resolver: the coefficient passes through for WD-DECAY, is zeroed for WD-NONE
0.5 WD-DECAY ADAMW-WD-FOR  100.0 f* 0.5 f+ f>s   50 T=
0.5 WD-NONE  ADAMW-WD-FOR  1000.0 f* 0.5 f+ f>s   0 T=

\ a 2D "weight" (WD-DECAY) and a "bias" (WD-NONE), same trainer wd=0.5
create PW-W 2 cells allot   create PW-R 2 cells allot   create PW-G 2 cells allot
create PW-M 2 cells allot   create PW-V 2 cells allot   create PW-RM 2 cells allot   create PW-RV 2 cells allot
create PB-W 2 cells allot   create PB-R 2 cells allot   create PB-G 2 cells allot
create PB-M 2 cells allot   create PB-V 2 cells allot   create PB-RM 2 cells allot   create PB-RV 2 cells allot

1.0 PW-W 0 T-SET   2.0 PW-W 1 T-SET   1.0 PW-R 0 T-SET   2.0 PW-R 1 T-SET
1.0 PW-G 0 T-SET   2.0 PW-G 1 T-SET
0.0 PW-M 0 T-SET   0.0 PW-M 1 T-SET   0.0 PW-V 0 T-SET   0.0 PW-V 1 T-SET
0.0 PW-RM 0 T-SET  0.0 PW-RM 1 T-SET  0.0 PW-RV 0 T-SET  0.0 PW-RV 1 T-SET

0.5 PB-W 0 T-SET   0.0 0.5 f- PB-W 1 T-SET   0.5 PB-R 0 T-SET   0.0 0.5 f- PB-R 1 T-SET
0.5 PB-G 0 T-SET   0.0 0.5 f- PB-G 1 T-SET
0.0 PB-M 0 T-SET   0.0 PB-M 1 T-SET   0.0 PB-V 0 T-SET   0.0 PB-V 1 T-SET
0.0 PB-RM 0 T-SET  0.0 PB-RM 1 T-SET  0.0 PB-RV 0 T-SET  0.0 PB-RV 1 T-SET

\ weight group: policy resolves wd=0.5 -> AdamW; reference is plain Adam
0.1 0.9 0.999 0.0 0.1 0.001  0.5 WD-DECAY ADAMW-WD-FOR  PW-W PW-G PW-M PW-V 2 OPTIM:TT-ADAMW!
0.1 0.9 0.999 0.0 0.1 0.001  PW-R PW-G PW-RM PW-RV 2 OPTIM:TT-ADAM!
PW-W 0 T-GET PW-R 0 T-GET f= TFALSE              \ 2D weight IS decayed
PW-W 1 T-GET PW-R 1 T-GET f= TFALSE

\ bias group: policy resolves wd=0.0 -> bit-identical to plain Adam
0.1 0.9 0.999 0.0 0.1 0.001  0.5 WD-NONE ADAMW-WD-FOR  PB-W PB-G PB-M PB-V 2 OPTIM:TT-ADAMW!
0.1 0.9 0.999 0.0 0.1 0.001  PB-R PB-G PB-RM PB-RV 2 OPTIM:TT-ADAM!
PB-W 0 T-GET PB-R 0 T-GET f= TTRUE               \ bias UNTOUCHED by wd
PB-W 1 T-GET PB-R 1 T-GET f= TTRUE

\ ================= red guards: named throws ====================================
create TB 1 cells allot
: AW-BAD-WD   ( -- )  0.1 0.9 0.999 0.0 0.1 0.001  0.0 0.5 f-  TB TB TB TB 1 OPTIM:TT-ADAMW! ;
: AW-BAD-FLAG ( -- )  0.5 7 ADAMW-WD-FOR drop ;
' AW-BAD-WD   E-ADAMW-WD     TTHROWS
' AW-BAD-FLAG E-ADAMW-POLICY TTHROWS

T-REPORT

;package

\ maki/mlp-test.f - train the 2-layer MLP on the CPU host and assert it learns.
\
\ in=2, hid=2, out=1, batch=2. Fixed init + a fixed target; run SGD and require the
\ MSE loss to fall (the gradients are op-level gradchecked elsewhere, so this proves
\ the forward/backward/SGD are wired end-to-end and the model descends).

T-RESET

create QX  4 cells allot   create MT  2 cells allot
create MW1 4 cells allot   create MB1 2 cells allot
create MW2 2 cells allot   create MB2 1 cells allot
create MZ1 4 cells allot   create MH  4 cells allot   create QY 2 cells allot
create QDY 2 cells allot   create MDH 4 cells allot   create MDZ1 4 cells allot
create QDX 4 cells allot   create MDW1 4 cells allot  create MDB1 2 cells allot
create MDW2 2 cells allot  create MDB2 1 cells allot

variable L0   variable LF

: MLP-INIT ( -- )
   1.0 QX 0 T-SET  2.0 QX 1 T-SET  3.0 QX 2 T-SET  4.0 QX 3 T-SET   \ X 2x2
   1.0 MT 0 T-SET  0.0 MT 1 T-SET                                   \ target 2x1
   0.1 MW1 0 T-SET  0.2 MW1 1 T-SET  0.3 MW1 2 T-SET  0.4 MW1 3 T-SET
   0.0 MB1 2 T-FILL
   0.5 MW2 0 T-SET  0.6 MW2 1 T-SET
   0.0 MB2 1 T-FILL ;

: MFWD  ( -- )  QX MW1 MB1 MW2 MB2 MZ1 MH QY  2 2 2 1  MLP-FWD ;
: MLOSS ( -- r ) QY MT 2 TT-MSE-LOSS ;
: MEPOCH ( -- )                                         \ one SGD epoch at lr=0.01
   MFWD
   QY MT QDY 2 TT-MSE-DY
   QDY QX MW1 MZ1 MH MW2  MDH MDZ1 QDX MDW1 MDB1 MDW2 MDB2  2 2 2 1  MLP-BWD
   0.01  MW1 MB1 MW2 MB2  MDW1 MDB1 MDW2 MDB2  2 2 1  MLP-SGD ;
: TRAIN ( n -- )  0 ?do  MEPOCH  loop ;

MLP-INIT
MFWD  MLOSS  1000.0 f* 0.5 f+ f>s  L0 !                  \ initial loss x1000
200 TRAIN
MFWD  MLOSS  1000.0 f* 0.5 f+ f>s  LF !                  \ final loss x1000

LF @  L0 @  <        TTRUE                               \ loss decreased
LF @  2 *  L0 @  <   TTRUE                               \ ... by more than half (2*LF < L0)

T-REPORT

\ maki/maki-test.f - consumer fixture for the one-file Maki entry point.
\
\ Loads ONLY maki/maki.f (plus the Habu test/float stdlib the harness needs) and
\ drives a representative model-authoring / train / eval / report slice through
\ the curated MAKI: names, proving each is the IDENTICAL word as its subsystem
\ name (the EXPORT contract: one body, two names) and that it computes the same
\ result. It also drills into the subsystem packages directly (EVAL:, REPORT:) to
\ prove the second access mode maki/maki.f promises.

require maki/maki.f
require lib/test.f
require lib/float.f

T-RESET

\ --- alias identity: MAKI:WORD and PKG:WORD share one execution token ----------
\ Proven for every curated re-export, including model-import words that need a
\ real proto blob to run - identity is the exact parity contract EXPORT gives.

\ loss (scalar) + VJPs
' MAKI:MSE                ' LOSS:MSE                 =  TTRUE
' MAKI:MSE-GRAD           ' LOSS:MSE-GRAD            =  TTRUE
' MAKI:L1                 ' LOSS:L1                  =  TTRUE
' MAKI:NLL                ' LOSS:NLL                 =  TTRUE
' MAKI:NLL-MU-GRAD        ' LOSS:NLL-MU-GRAD         =  TTRUE
' MAKI:NLL-LOGVAR-GRAD    ' LOSS:NLL-LOGVAR-GRAD     =  TTRUE
' MAKI:MAHALANOBIS        ' LOSS:MAHALANOBIS         =  TTRUE
' MAKI:MAHALANOBIS-GRAD   ' LOSS:MAHALANOBIS-GRAD    =  TTRUE
' MAKI:HUBER              ' LOSS:HUBER               =  TTRUE
' MAKI:HUBER-GRAD         ' LOSS:HUBER-GRAD          =  TTRUE
' MAKI:CE                 ' LOSS:CE                  =  TTRUE
' MAKI:SOFTMAX-CE-BWD     ' LOSS:SOFTMAX-CE-BWD      =  TTRUE

\ loss (tensor-graph) + cotangents
' MAKI:TT-MSE             ' LOSS:TT-MSE              =  TTRUE
' MAKI:TT-MSE-DY          ' LOSS:TT-MSE-DY           =  TTRUE
' MAKI:TT-NLL             ' LOSS:TT-NLL              =  TTRUE
' MAKI:TT-NLL-DMU         ' LOSS:TT-NLL-DMU          =  TTRUE
' MAKI:TT-NLL-DLV         ' LOSS:TT-NLL-DLV          =  TTRUE
' MAKI:TT-MAHALANOBIS     ' LOSS:TT-MAHALANOBIS      =  TTRUE
' MAKI:TT-MAHALANOBIS-DMU ' LOSS:TT-MAHALANOBIS-DMU  =  TTRUE
' MAKI:TT-HUBER           ' LOSS:TT-HUBER            =  TTRUE
' MAKI:TT-HUBER-DY        ' LOSS:TT-HUBER-DY         =  TTRUE

\ optimizer steps
' MAKI:SGD                ' OPTIM:SGD                =  TTRUE
' MAKI:SGD-MOM            ' OPTIM:SGD-MOM            =  TTRUE
' MAKI:WEIGHT-DECAY       ' OPTIM:WEIGHT-DECAY       =  TTRUE
' MAKI:ADAM               ' OPTIM:ADAM               =  TTRUE
' MAKI:ADAM-M             ' OPTIM:ADAM-M             =  TTRUE
' MAKI:ADAM-V             ' OPTIM:ADAM-V             =  TTRUE
' MAKI:ADAM-W             ' OPTIM:ADAM-W             =  TTRUE
' MAKI:TT-ADAM!           ' OPTIM:TT-ADAM!           =  TTRUE

\ model import
' MAKI:IMPORT             ' ONNX:IMPORT              =  TTRUE
' MAKI:IMPORT-FILE        ' ONNX:IMPORT-FILE         =  TTRUE

\ checker-as-judge eval core
' MAKI:CHECK-PASSES?      ' EVAL:CHECK-PASSES?       =  TTRUE
' MAKI:PASS@1?            ' EVAL:PASS@1?             =  TTRUE

\ --- run-through: the curated name computes the subsystem's result ------------
\ Float assertions scale + round (x f* 0.5 f+ f>s) to dodge binary-fp jitter,
\ exactly like maki/loss-test.f and maki/optim-test.f.

\ losses through MAKI:
0.5 0.0 MAKI:MSE              4.0 f* 0.5 f+ f>s   1 T=
0.5 0.0 MAKI:MSE-GRAD        4.0 f* 0.5 f+ f>s   4 T=
0.5 1.0 MAKI:L1              4.0 f* 0.5 f+ f>s   2 T=
1.0 0.5 0.0 MAKI:NLL         1000.0 f* 0.5 f+ f>s 125 T=
1.0 0.5 0.0 MAKI:NLL-MU-GRAD 1000.0 f* 0.5 f- f>s -500 T=

\ optimizer steps through MAKI:
1.0 0.5 0.5 MAKI:SGD             4.0 f* 0.5 f+ f>s   3 T=
0.5 2.0 0.25 MAKI:WEIGHT-DECAY   4.0 f* 0.5 f+ f>s   4 T=
1.0 0.5 0.5 0.5 0.5 MAKI:SGD-MOM
   4.0 f* 0.5 f+ f>s   3 T=      \ v'
   8.0 f* 0.5 f+ f>s   5 T=      \ w'
0.0 1.0 0.9 MAKI:ADAM-M          10.0 f* 0.5 f+ f>s   1 T=
0.0 1.0 0.999 MAKI:ADAM-V        1000.0 f* 0.5 f+ f>s 1 T=
1.0 0.1 0.001 0.1 0.0 0.1 0.001 MAKI:ADAM-W  10.0 f* 0.5 f+ f>s 9 T=
1.0 1.0 0.0 0.0 0.1 0.9 0.999 0.0 0.1 0.001 MAKI:ADAM
   1000.0 f* 0.5 f+ f>s   1 T=   \ v'
   10.0 f* 0.5 f+ f>s     1 T=   \ m'
   10.0 f* 0.5 f+ f>s     9 T=   \ w'

\ --- eval: curated judge word + drill-in tally --------------------------------
s" GOOD-K ( n -- n ) 1+"          MAKI:CHECK-PASSES?  TTRUE
s" BAD-K ( n -- n n ) drop"       MAKI:CHECK-PASSES?  TFALSE

EVAL:RESET                                            \ drill into EVAL: directly
s" A ( n -- n ) 1+"        EVAL:SCORE
s" B ( n -- n n ) drop"    EVAL:SCORE
s" C ( n -- n ) dup drop"  EVAL:SCORE
EVAL:TOTAL @  3 T=
EVAL:PASS  @  2 T=
MAKI:PASS@1?  TTRUE                                   \ curated metric, same tally

EVAL:RESET
s" D ( n -- ) dup"         EVAL:SCORE
MAKI:PASS@1?  TFALSE

\ --- capture a model report + render it (REPORT: drill-in) --------------------
REPORT:NEW
   s" FFN"   REPORT:MODEL!
   s" sm_87" REPORT:TARGET!
   dup REPORT:MODEL$   s" FFN"   CONTAINS?  TTRUE
   dup REPORT:TARGET$  s" sm_87" CONTAINS?  TTRUE
   REPORT:RENDER-HUMAN s" FFN"   CONTAINS?  TTRUE

T-REPORT
s" maki: ok" type cr

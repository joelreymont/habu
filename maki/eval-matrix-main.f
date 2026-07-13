\ maki/eval-matrix-main.f - the durable eval-matrix entry point.
\
\ Replays generation-transcript files (docs/maki/eval.md format v1) through the
\ committed checker judge and prints the eval matrix (superset of the docs/eval-triton.md recorded table):
\
\   bin/hb --load maki/eval-matrix-main.f -- run1.txt run2.txt ...
\
\ With no arguments it replays the committed synthetic fixtures under
\ maki/transcripts/ - the deterministic self-demonstration the maki suite runs.
\ Future pass@k rounds: record each generation arm as a transcript file, then
\ run this exact command - no /tmp scripts, no ad hoc logs. GB/s + device
\ verdict columns stay `not-run` on the host; on the Orin the bandwidth bench
\ and device graders fill them through EVAL:MATRIX-GBS! / EVAL:MATRIX-DEVICE!.

require lib/ptx/test-prelude.f
\ the replay judge image must carry EVERY committed task's authoring surface:
\ the checker rejects unknown vocabulary, so a missing require silently grades
\ that task 0 green (the gemm/attention phase words live outside test-prelude)
require lib/ptx/cg-matmul.f
require lib/ptx/cg-attention.f
require maki/eval-matrix.f

package EVAL

private

2 constant VOCAB-STDERR-FD

defer VOCAB-WORD? ( ptr u8 n -- bool )

: VOCAB-LIVE? ( ptr u8 n -- bool )
   CHECKER-DEFINED? ;

: VOCAB-LIVE! ( -- )
   [: VOCAB-LIVE? ;] is VOCAB-WORD? ;

VOCAB-LIVE!

: VOCAB-FAIL ( -- )
   SB$ {: a:ptr u:n :}
   VOCAB-STDERR-FD a u write u <> if E-EVAL-VOCAB throw then
   E-EVAL-VOCAB throw ;

: VOCAB-MISSING-WORD ( ptr u8 n ptr u8 n -- )
   {: task:ptr tasku:n word:ptr wordu:n :}
   SB-RESET
   s" eval-matrix: missing vocabulary for " SB-APPEND
   task tasku SB-APPEND
   s" : " SB-APPEND
   word wordu SB-APPEND
   STR-LF SB-APPEND-C
   VOCAB-FAIL ;

: VOCAB-NEED ( ptr u8 n ptr u8 n -- )
   {: task:ptr tasku:n word:ptr wordu:n :}
   word wordu VOCAB-WORD? if exit then
   task tasku word wordu VOCAB-MISSING-WORD ;

: VOCAB-SAXPY ( -- )
   s" saxpy" s" GRID-CTX"  VOCAB-NEED
   s" saxpy" s" LOAD"      VOCAB-NEED
   s" saxpy" s" SCALE"     VOCAB-NEED
   s" saxpy" s" STORE"     VOCAB-NEED ;

: VOCAB-COLLECTIVE ( -- )
   s" sumnorm" s" ROW"         VOCAB-NEED
   s" sumnorm" s" ROW-SPAN"    VOCAB-NEED
   s" sumnorm" s" ROW-CTX"     VOCAB-NEED
   s" sumnorm" s" ROW-LOAD"    VOCAB-NEED
   s" sumnorm" s" BLOCK-SUM"   VOCAB-NEED
   s" sumnorm" s" PTX:B/"      VOCAB-NEED
   s" sumnorm" s" ROW-STORE"   VOCAB-NEED
   s" softmax" s" BLOCK-MAX"   VOCAB-NEED
   s" softmax" s" PTX:B-"      VOCAB-NEED
   s" softmax" s" EXP."        VOCAB-NEED ;

: VOCAB-FUSED-RELU ( -- )
   s" fused-relu" s" GRID-CTX-V4"  VOCAB-NEED
   s" fused-relu" s" LOAD-V4"      VOCAB-NEED
   s" fused-relu" s" SCALE-V4"     VOCAB-NEED
   s" fused-relu" s" ADD-V4"       VOCAB-NEED
   s" fused-relu" s" RELU-V4"      VOCAB-NEED
   s" fused-relu" s" STORE-V4"     VOCAB-NEED ;

: VOCAB-GEMM ( -- )
   s" gemm" s" MM-BEGIN"   VOCAB-NEED
   s" gemm" s" MM-K-LOOP"  VOCAB-NEED
   s" gemm" s" MM-STORE"   VOCAB-NEED ;

: VOCAB-ATTN ( -- )
   s" attention" s" ATTN:START"    VOCAB-NEED
   s" attention" s" ATTN:STAGE-Q"  VOCAB-NEED
   s" attention" s" ATTN:SCORE"    VOCAB-NEED
   s" attention" s" ATTN:SOFTMAX"  VOCAB-NEED
   s" attention" s" ATTN:OUTPUT"   VOCAB-NEED
   s" attention" s" ATTN:FINISH"   VOCAB-NEED ;

: VOCAB-TASK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" saxpy"      STR= if true exit then
   a u s" softmax"    STR= if true exit then
   a u s" fused-relu" STR= if true exit then
   a u s" sumnorm"    STR= if true exit then
   a u s" gemm"       STR= if true exit then
   a u s" attention"  STR= ;

: VOCAB-MISSING-TASK ( ptr u8 n -- ) {: a:ptr u:n :}
   SB-RESET
   s" eval-matrix: task has no vocabulary manifest: " SB-APPEND
   a u SB-APPEND
   STR-LF SB-APPEND-C
   VOCAB-FAIL ;

: VOCAB-TASKS-CHECK ( -- )
   TS-TASKS@ 0 ?do
      i TS-TASK$ 2dup VOCAB-TASK? 0= if VOCAB-MISSING-TASK else 2drop then
   loop ;

public

: EM-VOCAB-CHECK ( -- )
   VOCAB-SAXPY
   VOCAB-COLLECTIVE
   VOCAB-FUSED-RELU
   VOCAB-GEMM
   VOCAB-ATTN ;

private

: EM-REPLAY ( ptr u8 n -- )
   TS-FILE
   VOCAB-TASKS-CHECK
   MATRIX-FROM-TS ;

: EM-FIXTURES ( -- )
   s" maki/transcripts/synth-habu-ptx.txt" EM-REPLAY
   s" maki/transcripts/synth-triton.txt" EM-REPLAY ;

public

: EM-MAIN ( -- )
   EM-VOCAB-CHECK
   MATRIX-RESET
   SCRIPT-ARGC 0 > if
      SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ EM-REPLAY loop
   else
      EM-FIXTURES
   then
   MATRIX-RENDER type ;

;package

EVAL:EM-MAIN

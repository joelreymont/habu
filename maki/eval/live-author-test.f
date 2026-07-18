\ maki/eval/live-author-test.f - replay the committed LIVE 2026-07-13 round
\ over the three OFF-DEVICE authoring tasks (sumnorm / gemm / attention).
\
\ maki/transcripts/live-habu-ptx-2026-07-13.txt is verbatim output of live
\ opus-model generator sessions (blind, n=5 per task, at most one diagnostic-
\ guided repair round; none was needed - see the transcript header). Replaying
\ it through the committed checker judge pins the graded tallies: 5/5
\ first-try green on every task, zero repair rounds, with tokens-to-green in
\ both units (whitespace proxy + GEN-TOK-EST). Each DISTINCT live candidate
\ shape is additionally graded through the emit-structural autograder
\ (maki/eval/emit.f, candidate text copied verbatim from the transcript), so
\ GREEN(2) here proves certify + child-process emit + PTX structural gates on
\ real model output. The device-golden leg of these tasks is Orin-gated and
\ recorded as a SKIP (device-FFI SKIP pattern, maki/device-smoke.f).
\ INTENDED COUPLING: grading is LIVE through the CURRENT checker + emitters,
\ so a checker or emitter change that flips any committed verdict fails this
\ suite loudly (re-grade + re-pin with the change that legitimately moved it,
\ or fix the regression) - same tripwire as maki/eval/live-test.f.

require lib/ptx/test-prelude.f
require maki/eval/matrix.f
require maki/eval/emit.f

package MAKI

variable LAA  variable LAU
: LASAVE ( ptr u8 n -- )  LAU ! LAA ! ;
: LAHAS ( ptr u8 n -- )  LAA @ LAU @ 2swap CONTAINS? TTRUE ;

: EVAL-LIVE-AUTHOR-MAIN ( -- )
   T-RESET

   \ --- replayed tallies: the checker as judge over the live candidates ---
   s" maki/transcripts/live-habu-ptx-2026-07-13.txt" EVAL:TS-FILE
   EVAL:TS-TARGET$ s" habu-ptx" T$=
   EVAL:TS-TASKS@ 3 T=
   0 EVAL:TS-TASK$ s" sumnorm" T$=
   0 EVAL:TS-N@        5 T=
   0 EVAL:TS-GREEN@    5 T=      \ n1..n5 first-try green
   0 EVAL:TS-REPAIRED@ 0 T=
   0 EVAL:TS-ROUNDS@   0 T=
   0 EVAL:TS-TOKENS@ 194 T=
   0 EVAL:TS-EST@    472 T=
   1 EVAL:TS-TASK$ s" gemm" T$=
   1 EVAL:TS-N@        5 T=
   1 EVAL:TS-GREEN@    5 T=      \ all five compose MM-BEGIN MM-K-LOOP MM-STORE
   1 EVAL:TS-REPAIRED@ 0 T=
   1 EVAL:TS-ROUNDS@   0 T=
   1 EVAL:TS-TOKENS@  50 T=
   1 EVAL:TS-EST@    320 T=
   2 EVAL:TS-TASK$ s" attention" T$=
   2 EVAL:TS-N@        5 T=
   2 EVAL:TS-GREEN@    5 T=      \ a1..a5 first-try green (phase pipeline)
   2 EVAL:TS-REPAIRED@ 0 T=
   2 EVAL:TS-ROUNDS@   0 T=
   2 EVAL:TS-TOKENS@  90 T=
   2 EVAL:TS-EST@    469 T=

   \ --- emit-structural grades over every DISTINCT live candidate shape ---
   \ sumnorm: five distinct shapes, all GREEN through checker+emit+structure
   s" live n1 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} in ROW ROW-SPAN {: si :} si ROW-CTX {: ctx :} si ctx ROW-LOAD {: t :} t t BLOCK-SUM PTX:B/ out ROW ROW-SPAN ctx ROW-STORE"
      EVAL:GRADE-SUMNORM 2 T=
   s" live n2 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: min mout :} min ROW ROW-SPAN {: sin :} mout ROW ROW-SPAN {: sout :} sin ROW-CTX {: ctx :} sin ctx ROW-LOAD {: t :} t BLOCK-SUM {: s :} t s PTX:B/ sout ctx ROW-STORE"
      EVAL:GRADE-SUMNORM 2 T=
   s" live n3 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: min mout :} min ROW ROW-SPAN ROW-CTX {: ctx :} min ROW ROW-SPAN ctx ROW-LOAD {: t :} t t BLOCK-SUM PTX:B/ mout ROW ROW-SPAN ctx ROW-STORE"
      EVAL:GRADE-SUMNORM 2 T=
   s" live n4 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: min mout :} min ROW ROW-SPAN {: sin :} sin ROW-CTX {: ctx :} sin ctx ROW-LOAD dup BLOCK-SUM PTX:B/ mout ROW ROW-SPAN ctx ROW-STORE"
      EVAL:GRADE-SUMNORM 2 T=
   s" live n5 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: min mout :} ROW {: r :} min r ROW-SPAN {: sin :} mout r ROW-SPAN {: sout :} sin ROW-CTX {: ctx :} sin ctx ROW-LOAD {: t :} t BLOCK-SUM {: s :} t s PTX:B/ sout ctx ROW-STORE"
      EVAL:GRADE-SUMNORM 2 T=
   \ gemm: one shape shared by g1..g5
   s" live g1-g5 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) MM-BEGIN MM-K-LOOP MM-STORE"
      EVAL:GRADE-GEMM 2 T=
   \ attention: the point-free shape (a1/a2/a4) and the locals shape (a3/a5)
   s" live a1 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH"
      EVAL:GRADE-ATTN 2 T=
   s" live a3 grade" T-LABEL
      s" K ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) {: q k v o :} q k v o ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH"
      EVAL:GRADE-ATTN 2 T=

   \ --- the rendered matrix rows the docs section quotes ---
   EVAL:MATRIX-RESET
   EVAL:MATRIX-FROM-TS
   EVAL:MATRIX-RENDER LASAVE
   s" | sumnorm | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 194 | 472 | not-run | not-run | checker | proxy |" LAHAS
   s" | gemm | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 50 | 320 | not-run | not-run | checker | proxy |" LAHAS
   s" | attention | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 90 | 469 | not-run | not-run | checker | proxy |" LAHAS

   s" eval-live-author: 2026-07-13 round graded at checker/emit level -> device leg SKIPPED (Orin-gated)" type cr

   T-REPORT ;

EVAL-LIVE-AUTHOR-MAIN

;package

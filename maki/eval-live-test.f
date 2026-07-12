\ maki/eval-live-test.f - replay the committed LIVE 2026-07-12 generation round.
\
\ maki/transcripts/live-habu-ptx-2026-07-12.txt is verbatim output of live
\ claude-opus-4-8 generator sessions (blind, n=5 per task, at most one
\ diagnostic-guided repair round; see the transcript header). Replaying it
\ through the committed checker judge pins the graded tallies the
\ docs/eval-triton.md 2026-07-12 section reports: saxpy 4/5 first-try + 1
\ repaired green in 1 round, softmax 5/5 (corrected ROW-STORE prompt),
\ fused-relu 5/5. The transcript is a static committed file, so every pin is
\ deterministic replay of that file - no model variance in this suite.
\ INTENDED COUPLING: grading is LIVE through the CURRENT checker, so these pins
\ are also a checker-regression tripwire - a checker change that flips any
\ committed candidate's verdict fails this suite loudly (re-grade + re-pin with
\ the change that legitimately moved it, or fix the checker regression).

require lib/ptx/test-prelude.f
require maki/eval-matrix.f

package MAKI

variable LVA  variable LVU
: LVSAVE ( ptr u8 n -- )  LVU ! LVA ! ;
: LVHAS ( ptr u8 n -- )  LVA @ LVU @ 2swap CONTAINS? TTRUE ;

: EVAL-LIVE-MAIN ( -- )
   T-RESET

   \ --- replayed tallies: the checker as judge over the live candidates ---
   s" maki/transcripts/live-habu-ptx-2026-07-12.txt" EVAL:TS-FILE
   EVAL:TS-TARGET$ s" habu-ptx" T$=
   EVAL:TS-TASKS@ 3 T=
   0 EVAL:TS-TASK$ s" saxpy" T$=
   0 EVAL:TS-N@        5 T=
   0 EVAL:TS-GREEN@    4 T=      \ sx2..sx5 first-try green
   0 EVAL:TS-REPAIRED@ 1 T=      \ sx1: mask-mismatch reject at '+.'
   0 EVAL:TS-ROUNDS@   1 T=      \ ...repaired green in one round
   0 EVAL:TS-TOKENS@ 172 T=
   0 EVAL:TS-REC@      0 T=
   1 EVAL:TS-TASK$ s" softmax" T$=
   1 EVAL:TS-N@        5 T=
   1 EVAL:TS-GREEN@    5 T=
   1 EVAL:TS-REPAIRED@ 0 T=
   1 EVAL:TS-ROUNDS@   0 T=
   1 EVAL:TS-TOKENS@ 292 T=
   2 EVAL:TS-TASK$ s" fused-relu" T$=
   2 EVAL:TS-N@        5 T=
   2 EVAL:TS-GREEN@    5 T=
   2 EVAL:TS-REPAIRED@ 0 T=
   2 EVAL:TS-ROUNDS@   0 T=
   2 EVAL:TS-TOKENS@ 150 T=

   \ --- the rendered matrix rows the docs section quotes ---
   EVAL:MATRIX-RESET
   EVAL:MATRIX-FROM-TS
   EVAL:MATRIX-RENDER LVSAVE
   s" | saxpy | habu-ptx | 5 | 4 | 800 | 1000 | 1000 | 1 | 1 | 172 | not-run | not-run | checker |" LVHAS
   s" | softmax | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 292 | not-run | not-run | checker |" LVHAS
   s" | fused-relu | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 150 | not-run | not-run | checker |" LVHAS

   T-REPORT ;

EVAL-LIVE-MAIN

;package

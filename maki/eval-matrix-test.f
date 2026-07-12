\ maki/eval-matrix-test.f - the matrix report over both committed fixtures.
\
\ Replays maki/transcripts/synth-habu-ptx.txt (live checker grading) and
\ synth-triton.txt (recorded external verdicts) into one matrix, then asserts
\ the rendered docs/eval-triton.md schema: exact pass@k per-mille cells,
\ repair/tokens columns, HONEST not-run GB/s + device placeholders on the
\ host, the device/GB/s setters (the Orin wiring), and every fail-closed
\ class (duplicate row, bad index, bad value).

require lib/ptx/test-prelude.f
require maki/eval-matrix.f

package MAKI

\ ---- render containment (report-test.f pattern) ----
variable MXVA  variable MXVU
: MXSAVE ( ptr u8 n -- )  MXVU ! MXVA ! ;
: MXHAS ( ptr u8 n -- )  MXVA @ MXVU @ 2swap CONTAINS? TTRUE ;
: MXHASNT ( ptr u8 n -- )  MXVA @ MXVU @ 2swap CONTAINS? TFALSE ;

: MXT-LOAD ( -- )
   EVAL:MATRIX-RESET
   s" maki/transcripts/synth-habu-ptx.txt" EVAL:TS-FILE
   EVAL:MATRIX-FROM-TS
   s" maki/transcripts/synth-triton.txt" EVAL:TS-FILE
   EVAL:MATRIX-FROM-TS ;

: MXT-DUP ( -- )
   MXT-LOAD
   s" maki/transcripts/synth-triton.txt" EVAL:TS-FILE
   EVAL:MATRIX-FROM-TS ;

: MXT-BAD-IDX ( -- )  MXT-LOAD 9 EVAL:MATRIX-N@ drop ;
: MXT-BAD-GBS ( -- )  MXT-LOAD 0 -5 EVAL:MATRIX-GBS! ;
: MXT-BAD-DEV ( -- )  MXT-LOAD 0 9 EVAL:MATRIX-DEVICE! ;

: EVAL-MATRIX-MAIN ( -- )
   T-RESET

   \ --- rows assembled from both fixture arms ---
   MXT-LOAD
   EVAL:MATRIX-ROWS@ 4 T=
   0 EVAL:MATRIX-TASK$   s" saxpy"    T$=
   0 EVAL:MATRIX-TARGET$ s" habu-ptx" T$=
   1 EVAL:MATRIX-TASK$   s" softmax"  T$=
   2 EVAL:MATRIX-TARGET$ s" triton"   T$=
   0 EVAL:MATRIX-N@ 5 T=
   1 EVAL:MATRIX-GREEN@ 3 T=
   3 EVAL:MATRIX-GREEN@ 5 T=

   \ --- rendered schema: the recorded pass@k round + honest placeholders ---
   EVAL:MATRIX-RENDER MXSAVE
   s" | task | target |" MXHAS
   s" | graded | tok-src |" MXHAS
   s" | saxpy | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | " MXHAS
   s" | softmax | habu-ptx | 5 | 3 | 600 | 900 | 1000 | 2 | 3 | " MXHAS
   s" | softmax | triton | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 0 | not-run | not-run | recorded |" MXHAS
   s" | not-run | not-run | checker | proxy |" MXHAS    \ v1 replay: proxy-marked tokens
   s" | not-run | not-run | recorded | - |" MXHAS       \ recorded arm: no token data
   s" | mixed |" MXHASNT

   \ --- device wiring: setters land in the row's columns ---
   0 630 EVAL:MATRIX-GBS!
   0 V-PASS EVAL:MATRIX-DEVICE!
   1 V-FAIL EVAL:MATRIX-DEVICE!
   EVAL:MATRIX-RENDER MXSAVE
   s"  | 630 | pass |" MXHAS
   s"  | fail |" MXHAS
   s" | saxpy | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | " MXHAS

   \ --- an empty-task row renders `-` pass cells instead of a fake number ---
   EVAL:MATRIX-RESET
   EVAL:TS-RESET
   s" habu-eval-transcript v1" EVAL:TS-LINE
   s" target habu-ptx" EVAL:TS-LINE
   s" task gemm" EVAL:TS-LINE
   EVAL:TS-END
   EVAL:MATRIX-FROM-TS
   EVAL:MATRIX-RENDER MXSAVE
   s" | gemm | habu-ptx | 0 | 0 | - | - | - | " MXHAS
   s" | not-run | not-run | - | - |" MXHAS

   \ --- a v1.1 model-token transcript renders the honest `model` marker ---
   EVAL:MATRIX-RESET
   EVAL:TS-RESET
   s" habu-eval-transcript v1.1" EVAL:TS-LINE
   s" target habu-ptx" EVAL:TS-LINE
   s" task saxpy" EVAL:TS-LINE
   s" sample m1" EVAL:TS-LINE
   s" candidate K ( n -- n ) 1+" EVAL:TS-LINE
   s" tokens 120" EVAL:TS-LINE
   EVAL:TS-END
   EVAL:MATRIX-FROM-TS
   EVAL:MATRIX-RENDER MXSAVE
   s" | saxpy | habu-ptx | 1 | 1 | 1000 | - | - | 0 | 0 | 120 | not-run | not-run | checker | model |" MXHAS

   \ --- fail closed ---
   ['] MXT-DUP     E-MX-DUP TTHROWS
   ['] MXT-BAD-IDX E-MX-IDX TTHROWS
   ['] MXT-BAD-GBS E-MX-ARG TTHROWS
   ['] MXT-BAD-DEV E-MX-ARG TTHROWS

   T-REPORT ;

EVAL-MATRIX-MAIN

;package

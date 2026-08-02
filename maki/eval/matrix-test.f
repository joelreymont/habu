\ maki/eval/matrix-test.f - the matrix report over both committed fixtures.
\
\ Replays maki/transcripts/synth-habu-ptx.txt (live checker grading) and
\ synth-triton.txt (recorded external verdicts) into one matrix, then asserts
\ the rendered docs/eval-triton.md schema: exact pass@k per-mille cells,
\ repair/tokens columns, HONEST not-run GB/s + device placeholders on the
\ host, the device/GB/s setters (the Orin wiring), and every fail-closed
\ class (duplicate row, bad index, bad value).

require lib/ptx/test-prelude.f
require maki/eval/matrix-main.f

T-RESET

package EVAL

PTR-VARIABLE TEST-MISS-A
variable TEST-MISS-U

: TEST-MISS! ( ptr u8 n -- )
   TEST-MISS-U !
   TEST-MISS-A ! ;

: TEST-MISS$ ( -- ptr u8 n )
   TEST-MISS-A @ TEST-MISS-U @ ;

: TEST-MISS? ( ptr u8 n -- bool )
   TEST-MISS$ STR= 0= ;

: TEST-MISSING-VOCAB ( ptr u8 n -- )
   TEST-MISS!
   [: TEST-MISS? ;] is VOCAB-WORD?
   [: EM-VOCAB-CHECK ;] catch {: rc:n :}
   VOCAB-LIVE!
   rc throw ;

: TEST-MISSING-SAXPY ( -- )
   s" +." TEST-MISSING-VOCAB ;

: TEST-MISSING-GEMM ( -- )
   s" MM-K-LOOP" TEST-MISSING-VOCAB ;

: TEST-MISSING-ATTN ( -- )
   s" ATTN:SCORE" TEST-MISSING-VOCAB ;

: TEST-UNKNOWN-TASK ( -- )
   TS-RESET
   s" habu-eval-transcript" TS-LINE
   s" target habu-ptx" TS-LINE
   s" task future-kernel" TS-LINE
   TS-END
   VOCAB-TASKS-CHECK ;

' TEST-MISSING-SAXPY E-EVAL-VOCAB TTHROWS
' TEST-MISSING-GEMM  E-EVAL-VOCAB TTHROWS
' TEST-MISSING-ATTN E-EVAL-VOCAB TTHROWS
' TEST-UNKNOWN-TASK E-EVAL-VOCAB TTHROWS

;package

EVAL:EM-VOCAB-CHECK

package EVAL

\ Restore the unloaded entry-module state for the following resident-suite row.
private
undefine VOCAB-STDERR-FD
undefine VOCAB-WORD?
undefine VOCAB-LIVE?
undefine VOCAB-LIVE!
undefine VOCAB-FAIL
undefine VOCAB-MISSING-WORD
undefine VOCAB-NEED
undefine VOCAB-SAXPY
undefine VOCAB-COLLECTIVE
undefine VOCAB-FUSED-RELU
undefine VOCAB-GEMM
undefine VOCAB-ATTN
undefine VOCAB-TASK?
undefine VOCAB-MISSING-TASK
undefine VOCAB-TASKS-CHECK
undefine EM-REPLAY
undefine EM-FIXTURES
public
undefine EM-VOCAB-CHECK
undefine EM-MAIN

;package

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
   s" | tokens-to-green | tok-est | GB/s-x10 |" MXHAS
   s" | graded | tok-src |" MXHAS
   s" | saxpy | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 156 | 321 | not-run |" MXHAS
   s" | softmax | habu-ptx | 5 | 3 | 600 | 900 | 1000 | 2 | 3 | 460 | 1010 | not-run |" MXHAS
   s" | softmax | triton | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 0 | 0 | not-run | not-run | recorded |" MXHAS
   s" | not-run | not-run | checker | proxy |" MXHAS    \ proxy-marked tokens
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
   s" habu-eval-transcript" EVAL:TS-LINE
   s" target habu-ptx" EVAL:TS-LINE
   s" task gemm" EVAL:TS-LINE
   EVAL:TS-END
   EVAL:MATRIX-FROM-TS
   EVAL:MATRIX-RENDER MXSAVE
   s" | gemm | habu-ptx | 0 | 0 | - | - | - | " MXHAS
   s" | not-run | not-run | - | - |" MXHAS

   \ --- a model-token transcript renders the honest `model` marker ---
   EVAL:MATRIX-RESET
   EVAL:TS-RESET
   s" habu-eval-transcript" EVAL:TS-LINE
   s" target habu-ptx" EVAL:TS-LINE
   s" task saxpy" EVAL:TS-LINE
   s" sample m1" EVAL:TS-LINE
   s" candidate K ( n -- n ) 1+" EVAL:TS-LINE
   s" tokens 120" EVAL:TS-LINE
   EVAL:TS-END
   EVAL:MATRIX-FROM-TS
   EVAL:MATRIX-RENDER MXSAVE
   s" | saxpy | habu-ptx | 1 | 1 | 1000 | - | - | 0 | 0 | 120 | 9 | not-run | not-run | checker | model |" MXHAS

   \ --- fail closed ---
   ['] MXT-DUP     E-MX-DUP TTHROWS
   ['] MXT-BAD-IDX E-MX-IDX TTHROWS
   ['] MXT-BAD-GBS E-MX-ARG TTHROWS
   ['] MXT-BAD-DEV E-MX-ARG TTHROWS

   T-REPORT ;

EVAL-MATRIX-MAIN

;package

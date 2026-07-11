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
require maki/eval-matrix.f

package EVAL

: EM-REPLAY ( ptr u8 n -- )
   TS-FILE
   MATRIX-FROM-TS ;

: EM-FIXTURES ( -- )
   s" maki/transcripts/synth-habu-ptx.txt" EM-REPLAY
   s" maki/transcripts/synth-triton.txt" EM-REPLAY ;

public

: EM-MAIN ( -- )
   MATRIX-RESET
   SCRIPT-ARGC 0 > if
      SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ EM-REPLAY loop
   else
      EM-FIXTURES
   then
   MATRIX-RENDER type ;

end-package

EVAL:EM-MAIN

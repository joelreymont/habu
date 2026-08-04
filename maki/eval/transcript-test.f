\ maki/eval/transcript-test.f - transcript replay over the committed synthetic
\ fixtures plus every fail-closed parse class.
\
\ Replays maki/transcripts/synth-habu-ptx.txt (live checker grading: SAXPY 5/5
\ first-try, softmax 3/5 first-try + 2 repaired green in 1 and 2 rounds) and
\ maki/transcripts/synth-triton.txt (recorded external verdicts), asserting the
\ deterministic tallies the matrix consumes. Needs the PTX tile + collective
\ vocab in-image so the candidate kernels check (host-only; no device).

require lib/ptx/test-prelude.f
require maki/eval/transcript.f

package MAKI

: TST-NO-HDR   ( -- )  EVAL:TS-RESET s" target habu-ptx" EVAL:TS-LINE ;
: TST-DUP-HDR  ( -- )
   EVAL:TS-RESET
   s" habu-eval-transcript" EVAL:TS-LINE
   s" habu-eval-transcript" EVAL:TS-LINE ;
: TST-END-NO-HDR ( -- )  EVAL:TS-RESET EVAL:TS-END ;
: TST-HDR      ( -- )  EVAL:TS-RESET s" habu-eval-transcript" EVAL:TS-LINE ;
: TST-UNKNOWN  ( -- )  TST-HDR s" verdict green" EVAL:TS-LINE ;
: TST-TASK-1ST ( -- )  TST-HDR s" task saxpy" EVAL:TS-LINE ;
: TST-2-TARGET ( -- )
   TST-HDR
   s" target habu-ptx" EVAL:TS-LINE
   s" target triton" EVAL:TS-LINE ;
: TST-SAMPLE-1ST ( -- )
   TST-HDR
   s" target habu-ptx" EVAL:TS-LINE
   s" sample s1" EVAL:TS-LINE ;
: TST-TO-TASK ( -- )
   TST-HDR
   s" target habu-ptx" EVAL:TS-LINE
   s" task saxpy" EVAL:TS-LINE ;
: TST-CAND-1ST ( -- )  TST-TO-TASK s" candidate K ( n -- n ) 1+" EVAL:TS-LINE ;
: TST-RESULT-1ST ( -- )  TST-TO-TASK s" result green" EVAL:TS-LINE ;
: TST-EMPTY-SAMPLE ( -- )
   TST-TO-TASK
   s" sample s1" EVAL:TS-LINE
   EVAL:TS-END ;
: TST-BAD-RESULT ( -- )
   TST-TO-TASK
   s" sample s1" EVAL:TS-LINE
   s" result maybe" EVAL:TS-LINE ;
\ a '|' in a task/target name would misalign the rendered matrix -> rejected
: TST-PIPE-TASK ( -- )
   TST-HDR
   s" target habu-ptx" EVAL:TS-LINE
   s" task a|evil" EVAL:TS-LINE ;
: TST-PIPE-TARGET ( -- )
   TST-HDR
   s" target ha|bu" EVAL:TS-LINE ;
: TST-MIXED ( -- )
   TST-TO-TASK
   s" sample s1" EVAL:TS-LINE
   s" candidate K ( n -- n ) 1+" EVAL:TS-LINE
   s" result green" EVAL:TS-LINE ;
: TST-NO-FILE ( -- )  s" maki/transcripts/no-such.txt" EVAL:TS-FILE ;
: TST-IDX-OOR ( -- )  2 EVAL:TS-N@ drop ;

\ ---- optional generator-reported `tokens N` per candidate ----------------
: TST-TOK-SAMPLE ( -- )
   TST-HDR
   s" target habu-ptx" EVAL:TS-LINE
   s" task saxpy" EVAL:TS-LINE
   s" sample m1" EVAL:TS-LINE ;
: TST-GREEN ( -- )  s" candidate K ( n -- n ) 1+" EVAL:TS-LINE ;
: TST-RED   ( -- )  s" candidate K ( n -- n ) drop" EVAL:TS-LINE ;
\ real path: every candidate model-counted; post-green extras stay ignored
: TST-MODEL ( -- )
   TST-TOK-SAMPLE
   TST-GREEN  s" tokens 120" EVAL:TS-LINE
   TST-GREEN  s" tokens 999" EVAL:TS-LINE    \ ignored: sample already green
   s" sample m2" EVAL:TS-LINE
   TST-RED    s" tokens 50" EVAL:TS-LINE     \ author reject -> 1 repair round
   TST-GREEN  s" tokens 60" EVAL:TS-LINE
   EVAL:TS-END ;
\ a transcript without tokens directives keeps the whitespace proxy
: TST-PROXY ( -- )
   TST-TOK-SAMPLE
   TST-GREEN
   s" sample m2" EVAL:TS-LINE
   TST-RED
   TST-GREEN
   EVAL:TS-END ;
\ fail-closed tokens classes
: TST-TOK-MISPLACED ( -- )  TST-TOK-SAMPLE s" tokens 120" EVAL:TS-LINE ;
: TST-TOK-DUP ( -- )
   TST-TOK-SAMPLE TST-GREEN
   s" tokens 120" EVAL:TS-LINE
   s" tokens 121" EVAL:TS-LINE ;
: TST-TOK-BAD-ARG  ( -- )  TST-TOK-SAMPLE TST-GREEN s" tokens 12x" EVAL:TS-LINE ;
: TST-TOK-ZERO-ARG ( -- )  TST-TOK-SAMPLE TST-GREEN s" tokens 0" EVAL:TS-LINE ;
: TST-TOK-NEG-ARG  ( -- )  TST-TOK-SAMPLE TST-GREEN s" tokens -5" EVAL:TS-LINE ;
\ mixed-file negatives: a transcript is all-model or all-proxy, never both
: TST-TOK-MIX-LATE ( -- )
   TST-TOK-SAMPLE
   TST-RED                                    \ no tokens -> proxy settled
   TST-GREEN  s" tokens 60" EVAL:TS-LINE ;
: TST-TOK-MIX-DROP ( -- )
   TST-TOK-SAMPLE
   TST-RED    s" tokens 50" EVAL:TS-LINE      \ model settled...
   TST-GREEN                                  \ ...then a proxy candidate
   EVAL:TS-END ;

: EVAL-TRANSCRIPT-MAIN ( -- )
   T-RESET

   \ --- replayed arm: the committed habu-ptx fixture, graded live ---
   s" maki/transcripts/synth-habu-ptx.txt" EVAL:TS-FILE
   EVAL:TS-TARGET$ s" habu-ptx" T$=
   EVAL:TS-TASKS@ 2 T=
   0 EVAL:TS-TASK$ s" saxpy" T$=
   0 EVAL:TS-N@        5 T=
   0 EVAL:TS-GREEN@    5 T=
   0 EVAL:TS-REPAIRED@ 0 T=
   0 EVAL:TS-ROUNDS@   0 T=
   0 EVAL:TS-REC@      0 T=
   0 EVAL:TS-TOKENS@ 0 > TTRUE
   0 EVAL:TS-EST@ 0 EVAL:TS-TOKENS@ > TTRUE   \ punct-heavy source: est > proxy
   1 EVAL:TS-TASK$ s" softmax" T$=
   1 EVAL:TS-N@        5 T=
   1 EVAL:TS-GREEN@    3 T=      \ pass@1 c: first-attempt greens only
   1 EVAL:TS-REPAIRED@ 2 T=      \ sh3 + sh4 reach green via repair
   1 EVAL:TS-ROUNDS@   3 T=      \ sh3: 2 rounds, sh4: 1 round
   1 EVAL:TS-REC@      0 T=
   0 EVAL:TS-TOKSRC@ EVAL:TS-TOK-PROXY T=   \ whitespace proxy, marked as such
   1 EVAL:TS-TOKSRC@ EVAL:TS-TOK-PROXY T=

   \ --- recorded arm: the committed triton fixture, no replay ---
   s" maki/transcripts/synth-triton.txt" EVAL:TS-FILE
   EVAL:TS-TARGET$ s" triton" T$=
   EVAL:TS-TASKS@ 2 T=
   0 EVAL:TS-N@     5 T=
   0 EVAL:TS-GREEN@ 5 T=
   0 EVAL:TS-REC@   5 T=
   1 EVAL:TS-REC@   5 T=
   1 EVAL:TS-TOKENS@ 0 T=
   1 EVAL:TS-EST@   0 T=
   1 EVAL:TS-TOKSRC@ EVAL:TS-TOK-NONE T=    \ recorded-only: no token data at all

   \ --- model-token path: `tokens` overrides the whitespace proxy ---
   TST-MODEL
   0 EVAL:TS-N@        2 T=
   0 EVAL:TS-GREEN@    1 T=
   0 EVAL:TS-REPAIRED@ 1 T=
   0 EVAL:TS-ROUNDS@   1 T=
   0 EVAL:TS-TOKENS@ 230 T=     \ m1 120 + m2 (50+60); the post-green 999 ignored
   0 EVAL:TS-EST@     26 T=     \ est stays source-derived: m1 9 + m2 (8+9)
   0 EVAL:TS-TOKSRC@ EVAL:TS-TOK-MODEL T=

   \ --- proxy path: no tokens directives -> proxy tally, marked proxy ---
   TST-PROXY
   0 EVAL:TS-N@        2 T=
   0 EVAL:TS-TOKENS@  21 T=     \ whitespace proxy: 7 + (7+7) source tokens
   0 EVAL:TS-EST@     26 T=     \ same est as the model path: unit-independent
   0 EVAL:TS-TOKSRC@ EVAL:TS-TOK-PROXY T=

   \ --- fail-closed parse classes ---
   ['] TST-NO-HDR       E-TS-HEADER TTHROWS
   ['] TST-DUP-HDR      E-TS-HEADER TTHROWS
   ['] TST-END-NO-HDR   E-TS-HEADER TTHROWS
   ['] TST-UNKNOWN      E-TS-LINE   TTHROWS
   ['] TST-TASK-1ST     E-TS-ORDER  TTHROWS
   ['] TST-2-TARGET     E-TS-ORDER  TTHROWS
   ['] TST-SAMPLE-1ST   E-TS-ORDER  TTHROWS
   ['] TST-CAND-1ST     E-TS-ORDER  TTHROWS
   ['] TST-RESULT-1ST   E-TS-ORDER  TTHROWS
   ['] TST-EMPTY-SAMPLE E-TS-EMPTY  TTHROWS
   ['] TST-BAD-RESULT   E-TS-LINE   TTHROWS
   ['] TST-PIPE-TASK    E-TS-LINE   TTHROWS
   ['] TST-PIPE-TARGET  E-TS-LINE   TTHROWS
   ['] TST-MIXED        E-TS-ORDER  TTHROWS
   ['] TST-NO-FILE      E-FS-OPEN   TTHROWS

   \ --- fail-closed tokens classes ---
   ['] TST-TOK-MISPLACED  E-TS-ORDER  TTHROWS
   ['] TST-TOK-DUP        E-TS-ORDER  TTHROWS
   ['] TST-TOK-BAD-ARG    E-TS-LINE   TTHROWS
   ['] TST-TOK-ZERO-ARG   E-TS-LINE   TTHROWS
   ['] TST-TOK-NEG-ARG    E-TS-LINE   TTHROWS
   ['] TST-TOK-MIX-LATE   E-TS-TOKENS TTHROWS
   ['] TST-TOK-MIX-DROP   E-TS-TOKENS TTHROWS

   \ --- index guard ---
   s" maki/transcripts/synth-triton.txt" EVAL:TS-FILE
   ['] TST-IDX-OOR E-TS-IDX TTHROWS

   T-REPORT ;

EVAL-TRANSCRIPT-MAIN

;package

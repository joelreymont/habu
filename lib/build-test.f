\ build-test.f - focused tests for checked build helpers.
\ Run: lib/build-test.sh

0 set-check

create BUILD-TEST-PATH FS-PATH-CAP allot

: BT-SRC ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;

: BT-MISSING ( -- ptr u8 n )
   1 SCRIPT-ARGV$ ;

: BT-ROOT ( -- ptr u8 n )
   2 SCRIPT-ARGV$ ;

: BT-CMD-OK ( -- ptr u8 n )
   3 SCRIPT-ARGV$ ;

: BT-CMD-NOART ( -- ptr u8 n )
   4 SCRIPT-ARGV$ ;

: BT-CMD-FAIL ( -- ptr u8 n )
   5 SCRIPT-ARGV$ ;

: BT-BAD-SRC ( -- ptr u8 n )
   6 SCRIPT-ARGV$ ;

: BT-UNCHECKABLE-SRC ( -- ptr u8 n )
   7 SCRIPT-ARGV$ ;

: BT-TOP-DIE-SRC ( -- ptr u8 n )
   8 SCRIPT-ARGV$ ;

: BT-ART ( -- ptr u8 n )
   BT-ROOT s" artifact.bin" BUILD-ARTIFACT ;

: BT-NOART ( -- ptr u8 n )
   BT-ROOT s" noart.bin" BUILD-ARTIFACT ;

: BT-OK-STEP ( -- n )
   0 ;

: BT-BAD-STEP ( -- n )
   7 ;

: BT-MISSING-SOURCE ( -- )
   BT-MISSING BUILD-CHECK ;

: BT-BAD-SOURCE ( -- )
   BT-BAD-SRC BUILD-CHECK ;

: BT-UNCHECKABLE-SOURCE ( -- )
   BT-UNCHECKABLE-SRC BUILD-CHECK ;

: BT-MISSING-EXPECT ( -- )
   BT-NOART BUILD-EXPECT ;

: BT-EMPTY-ARTIFACT ( -- )
   BT-ROOT s" " BUILD-ARTIFACT 2drop ;

: BT-BAD-STEP-RUN ( -- )
   s" bad-step" [: BT-BAD-STEP ;] BUILD-STEP ;

: BT-EMPTY-STEP ( -- )
   s" " [: BT-OK-STEP ;] BUILD-STEP ;

: BT-MISSING-COMMAND ( -- )
   BT-MISSING BT-ART BUILD-RUN drop ;

: BT-NO-ARTIFACT ( -- )
   BT-CMD-NOART BT-NOART BUILD-RUN drop ;

: BT-FAIL-COMMAND ( -- )
   BT-CMD-FAIL BT-ART BUILD-RUN drop ;

: BUILD-TEST-PATHS ( -- )
   BT-SRC BUILD-CHECK
   BT-TOP-DIE-SRC BUILD-CHECK
   ['] BT-MISSING-SOURCE E-BUILD-SOURCE TTHROWS
   ['] BT-BAD-SOURCE E-BUILD-SOURCE TTHROWS
   ['] BT-UNCHECKABLE-SOURCE E-BUILD-SOURCE TTHROWS
   BT-ROOT s" artifact.bin" BUILD-ARTIFACT
   BUILD-TEST-PATH swap
   BT-ROOT s" artifact.bin" BUILD-TEST-PATH JOIN-PATH
   BUILD-TEST-PATH swap T$=
   ['] BT-MISSING-EXPECT E-BUILD-PATH TTHROWS
   ['] BT-EMPTY-ARTIFACT E-BUILD-PATH TTHROWS ;

: BUILD-TEST-STEPS ( -- )
   s" ok-step" [: BT-OK-STEP ;] BUILD-STEP
   ['] BT-BAD-STEP-RUN E-BUILD-STATUS TTHROWS
   ['] BT-EMPTY-STEP E-BUILD-COMMAND TTHROWS ;

: BUILD-TEST-RUNS ( -- )
   BT-CMD-OK BT-ART BUILD-RUN 0 T=
   BT-ART FILE? TTRUE
   ['] BT-MISSING-COMMAND E-BUILD-COMMAND TTHROWS
   ['] BT-NO-ARTIFACT E-BUILD-PATH TTHROWS
   ['] BT-FAIL-COMMAND E-BUILD-STATUS TTHROWS ;

: BUILD-TEST-MAIN ( -- )
   T-RESET
   SCRIPT-ARGC 9 < if s" build-test: missing fixture args" T-EX-FAIL die then
   BUILD-TEST-PATHS
   BUILD-TEST-STEPS
   BUILD-TEST-RUNS
   T-REPORT
   s" build-test: ok" type cr ;

BUILD-TEST-MAIN

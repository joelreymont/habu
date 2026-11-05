\ run-cli-test.f - the native suite's command line: raw trailing tokens and
\ unknown test names are refused, and a chosen test runs alone.

require test/gate-common.f

package RUN-CLI-TEST

: RUN-BAD ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   s" test/run.f" GE-ARG+
   s" --bad" GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

: RUN-SEPARATED-BAD ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   s" test/run.f" GE-ARG+
   s" --" GE-ARG+
   s" --bad" GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

: RUN-CHOSEN ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   s" test/run.f" GE-ARG+
   s" --" GE-ARG+
   s" catch-frame" GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

: TEST ( -- )
   RUN-BAD
   64 s" native suite rejects arguments without separator" GE-EXPECT-RC
   s" usage: bin/hb --load test/run.f"
      s" native suite reports canonical invocation" GE-EXPECT-ERR-HAS
   RUN-SEPARATED-BAD
   64 s" native suite refuses an unknown test name" GE-EXPECT-RC
   s" no test named --bad"
      s" native suite names the unknown test" GE-EXPECT-ERR-HAS
   RUN-CHOSEN
   0 s" native suite runs a chosen test" GE-EXPECT-RC
   s" suites: ran 1 of "
      s" native suite runs only the chosen test" GE-EXPECT-OUT-HAS
   s" PASS: catch-frame"
      s" native suite runs the test it names" GE-EXPECT-OUT-HAS
   s" run-cli-test: ok" type cr ;

TEST

;package

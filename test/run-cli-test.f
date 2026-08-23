\ run-cli-test.f - canonical native-suite invocation rejects raw trailing tokens.

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

: TEST ( -- )
   RUN-BAD
   64 s" native suite rejects arguments without separator" GE-EXPECT-RC
   s" usage: bin/hb --load test/run.f"
      s" native suite reports canonical invocation" GE-EXPECT-ERR-HAS
   RUN-SEPARATED-BAD
   64 s" native suite rejects separated arguments" GE-EXPECT-RC
   s" run-cli-test: ok" type cr ;

TEST

;package

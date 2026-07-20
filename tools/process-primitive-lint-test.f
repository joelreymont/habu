\ process-primitive-lint-test.f - raw process primitive confinement coverage.

require lib/test.f
require tools/process-primitive-lint-core.f

package PROCESS-LINT-TEST

: GOOD ( -- )
   s" test/good.f"
   S\" \\ fork in comment\n: RUN ( -- ) s\q spawn-io\q 2drop PROC-FORK:CHECKED drop ;\n"
   PROCESS-LINT:SOURCE 0 T= ;

: BAD-SPAWN ( -- )
   s" test/bad.f" S\" : RUN ( -- ) 0 0 0 0 spawn-io drop ;\n"
   PROCESS-LINT:SOURCE 1 T= ;

: BAD-FORK ( -- )
   s" test/bad.f" S\" : RUN ( -- ) fork drop ;\n"
   PROCESS-LINT:SOURCE 1 T= ;

: BAD-TICK ( -- )
   s" test/bad.f" S\" : RUN ( -- ) ' fork execute ;\n"
   PROCESS-LINT:SOURCE 1 T=
   s" test/bad.f" S\" : RUN ( -- ) ['] spawn-io execute ;\n"
   PROCESS-LINT:SOURCE 1 T= ;

: ENGINE-REF ( -- )
   s" test/engine-suite.f"
   S\" s\" smoke-baked-word-resolves\" T-LABEL ' spawn-argv-env-cwd-io 0 <> -1 T=\n"
   PROCESS-LINT:SOURCE 0 T= ;

: ENGINE-BYPASS ( -- )
   s" test/engine-suite.f"
   S\" : BYPASS ( -- ) ' spawn-argv-env-cwd-io execute ;\ns\" smoke-baked-word-resolves\" T-LABEL ' spawn-argv-env-cwd-io 0 <> -1 T=\n"
   PROCESS-LINT:SOURCE 1 T= ;

: ENGINE-DUP ( -- )
   s" test/engine-suite.f"
   S\" s\" smoke-baked-word-resolves\" T-LABEL ' spawn-argv-env-cwd-io 0 <> -1 T=\ns\" smoke-baked-word-resolves\" T-LABEL ' spawn-argv-env-cwd-io 0 <> -1 T=\n"
   PROCESS-LINT:SOURCE 1 T= ;

: ALLOWED ( -- )
   s" lib/process.f" S\" : RAW ( -- ) fork drop ;\n"
   PROCESS-LINT:SOURCE 0 T=
   s" src/core/checker.f" S\" PRIM: fork PE-N PE-OUT PRIM;\n"
   PROCESS-LINT:SOURCE 0 T= ;

public

: TEST ( -- )
   T-RESET
   GOOD
   BAD-SPAWN
   BAD-FORK
   BAD-TICK
   ENGINE-REF
   ENGINE-BYPASS
   ENGINE-DUP
   ALLOWED
   T-REPORT
   s" process-primitive-lint-test: ok" type cr ;

;package

PROCESS-LINT-TEST:TEST

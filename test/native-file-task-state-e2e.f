\ FILE replay must retain a declared task's chain membership and exit hooks.
\ Bind a callback to an exposed task to exercise TASK-CHAIN, then join a task
\ whose registered exit hook turns its successful return into an error.
\ Run: bin/hb --load test/native-file-task-state-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TASK-STATE-TEST

: BUILD ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n product:ptr productu:n :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/native-unit-build.f" ARG s" --" ARG
   mode modeu ARG
   s" prefix.f" AT ARG s" task.f" AT ARG s" app.f" AT ARG
   s" task.unit" AT ARG product productu AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE SUCCESS ;

: RUN-IMAGE ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n expected:ptr expectedu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   product productu AT s" MAIN" CAPTURE SUCCESS
   OUT$ expected expectedu T$= ;

: CHAIN-SOURCE ( -- )
   s" task.f" S\" require src/lib/ffi-callback.f\npackage FILE-TASK\npublic\n$10000 TASK:TASK WORKER\nCALLBACK: ENTRY ( -- ) ;CALLBACK\n: RUN ( -- )\n   WORKER TASK:EXPOSE\n   ENTRY WORKER TASK:CONTEXT FFI-CB:ENTRY drop\n   WORKER TASK:UNEXPOSE WORKER TASK:KILL\n   s\q task chain ok\q type cr ;\n;package\n" PUT
   s" app.f" S\" require task.f\n: MAIN ( -- ) FILE-TASK:RUN ;\n" PUT ;

: EXIT-SOURCE ( -- )
   s" task.f" S\" require src/lib/task.f\npackage FILE-TASK\npublic\n$10000 TASK:TASK WORKER\n: EXIT-HOOK ( -- ) E-TASK-STATE throw ;\n' EXIT-HOOK WORKER TASK:AT-EXIT\n: BODY ( -- ) 7 TASK:RETURN ;\n: RUN ( -- )\n   ['] BODY WORKER TASK:ACTIVATE\n   WORKER TASK:JOIN MATCH result\n      ok OF drop s\q missing task exit hook\q 74 die ENDOF\n      err OF E-TASK-STATE <> if 74 throw then ENDOF\n   ;MATCH\n   s\q task exit ok\q type cr ;\n;package\n" PUT
   s" app.f" S\" require task.f\n: MAIN ( -- ) FILE-TASK:RUN ;\n" PUT ;

: CHAIN ( -- )
   s" cached task declarations remain in the callback context chain" T-LABEL
   s" prefix.f" S\" require src/lib/ffi-callback.f\n" PUT
   CHAIN-SOURCE
   s" --export-file" s" chain-export-hb" BUILD
   s" chain-export-hb" S\" task chain ok\n" RUN-IMAGE
   s" --import-file" s" chain-import-hb" BUILD
   OUT$ s" native-build: file unit hit" CONTAINS? TTRUE
   s" chain-import-hb" S\" task chain ok\n" RUN-IMAGE ;

: EXIT-HOOKS ( -- )
   s" cached task declarations retain their exit registrations" T-LABEL
   s" prefix.f" S\" require src/lib/task.f\n" PUT
   EXIT-SOURCE
   s" --export-file" s" exit-export-hb" BUILD
   s" exit-export-hb" S\" task exit ok\n" RUN-IMAGE
   s" --import-file" s" exit-import-hb" BUILD
   OUT$ s" native-build: file unit hit" CONTAINS? TTRUE
   s" exit-import-hb" S\" task exit ok\n" RUN-IMAGE ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-task-state" TREE
   CHAIN
   EXIT-HOOKS
   T-REPORT
   s" native FILE task artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-TASK-STATE-TEST:MAIN

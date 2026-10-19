\ FILE replay must retain a declared task's chain membership and exit hooks.
\ Bind a callback to an exposed task to exercise TASK-CHAIN, then join a task
\ whose registered exit hook turns its successful return into an error.
\ Run: bin/hb --load test/native-file-task-state-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require test/aot-maker-engine.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TASK-STATE-TEST

: BUILD ( ptr u8 n -- )
   {: cache:ptr cacheu:n :}
   AOT-MAKER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" 0" ARG s" 1" ARG
   maker makeru AOT-MAKER-ENGINE:STDIN$ CAPTURE SUCCESS ;

: RUN-IMAGE ( ptr u8 n -- )
   {: expected:ptr expectedu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" hb-aot-got" AT s" " CAPTURE SUCCESS
   OUT$ expected expectedu T$= ;

: COUNTS ( ptr u8 n ptr u8 n -- )
   {: hits:ptr hitsu:n compiled:ptr compiledu:n :}
   OUT$ hits hitsu CONTAINS? TTRUE
   OUT$ compiled compiledu CONTAINS? TTRUE ;

: CHAIN-SOURCE ( -- )
   s" task.f" S\" require src/lib/ffi-callback.f\npackage FILE-TASK\npublic\n$10000 TASK:TASK WORKER\nCALLBACK: ENTRY ( -- ) ;CALLBACK\n: RUN ( -- )\n   WORKER TASK:EXPOSE\n   ENTRY WORKER TASK:CONTEXT FFI-CB:ENTRY drop\n   WORKER TASK:UNEXPOSE WORKER TASK:KILL\n   s\q task chain ok\q type cr ;\n;package\n" PUT
   s" app.f" S\" require task.f\n: MAIN ( -- ) FILE-TASK:RUN ;\n" PUT ;

: EXIT-SOURCE ( -- )
   s" task.f" S\" require src/lib/task.f\npackage FILE-TASK\npublic\n$10000 TASK:TASK WORKER\n: EXIT-HOOK ( -- ) E-TASK-STATE throw ;\n' EXIT-HOOK WORKER TASK:AT-EXIT\n: BODY ( -- ) 7 TASK:RETURN ;\n: RUN ( -- )\n   ['] BODY WORKER TASK:ACTIVATE\n   WORKER TASK:JOIN MATCH result\n      ok OF drop s\q missing task exit hook\q 74 die ENDOF\n      err OF E-TASK-STATE <> if 74 throw then ENDOF\n   ;MATCH\n   s\q task exit ok\q type cr ;\n;package\n" PUT
   s" app.f" S\" require task.f\n: MAIN ( -- ) FILE-TASK:RUN ;\n" PUT ;

: CHAIN ( -- )
   s" cached task declarations remain in the callback context chain" T-LABEL
   s" chain-cache" AT MAKE-DIR
   CHAIN-SOURCE
   s" chain-cache" BUILD
   S\" file unit hits 0\n" S\" compiled 3\n" COUNTS
   S\" task chain ok\n" RUN-IMAGE
   s" chain-cache" BUILD
   S\" file unit hits 3\n" S\" compiled 0\n" COUNTS
   S\" task chain ok\n" RUN-IMAGE ;

: EXIT-HOOKS ( -- )
   s" cached task declarations retain their exit registrations" T-LABEL
   s" exit-cache" AT MAKE-DIR
   EXIT-SOURCE
   s" exit-cache" BUILD
   S\" file unit hits 0\n" S\" compiled 2\n" COUNTS
   S\" task exit ok\n" RUN-IMAGE
   s" exit-cache" BUILD
   S\" file unit hits 2\n" S\" compiled 0\n" COUNTS
   S\" task exit ok\n" RUN-IMAGE ;

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

\ gate-debug.f - checked runner for prop/debug gate checks.
\
\ Load after test/gate-common.f.

require tools/jitdump-core.f

: GDB-PROP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" test/prop-test.f" GE-SRC-FILE+
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" prop-test" GE-EXPECT-OK
   s" self-test OK" s" prop-test self-test/run did not complete" GE-EXPECT-OUT-HAS
   s" census OK" s" prop-test axiom census did not run in the gate path" GE-EXPECT-OUT-HAS
   s" alphabet OK" s" prop-test alphabet self-test did not run in the gate path" GE-EXPECT-OUT-HAS
   s" shard-seeds OK" s" prop-test shard-seed self-test did not run in the gate path" GE-EXPECT-OUT-HAS
   s" sweep-red OK" s" prop-test sweep red-path self-test did not run in the gate path" GE-EXPECT-OUT-HAS
   s" PASS: prop-test soundness smoke (self-hosted in habu, in-process via evaluate)" type cr ;

: GDB-PROFILER-SOURCE ( -- )
   GE-SRC-RESET
   s" : LONG-PROFILER-BUSY-WORD ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   s" : GO ( -- ) 100000 prof-on LONG-PROFILER-BUSY-WORD prof-report ;" GE-SRC-LINE
   s" GO" GE-SRC-LINE ;

: GDB-PROFILER ( -- )
   GE-HB-RESET
   GDB-PROFILER-SOURCE
   s" profiler long dictionary names" GE-HB-RUN-STDIN
   GT-OUT$ s" LONG-PROFILER-BUSY-WORD " STARTS-WITH? 0= if
      s" profiler long-name output" GE-FAIL
   then
   s" PASS: profiler long dictionary names" type cr ;

: GDB-JITDUMP ( -- )
   GE-HB-RESET
   [: s" : JITDUMP-SMOKE ( -- i64 ) 7 ;" JIT-EVALUATE
      s" JITDUMP-SMOKE" JIT-FIND JD ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC
   s" jitdump direct core" GE-EXPECT-OK
   s" ret" s" jitdump direct core output" GE-EXPECT-OUT-HAS
   s" PASS: jitdump direct core" type cr ;

: GDB-RUN ( -- )
   s" hb-gate-debug" GT-START
   GDB-PROP
   GDB-PROFILER
   GDB-JITDUMP
   GT-CLEANUP
   s" PASS: native prop/debug gate phase" type cr ;

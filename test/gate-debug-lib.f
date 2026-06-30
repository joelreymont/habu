\ gate-debug.f - checked runner for prop/debug gate checks.
\
\ Load after test/gate-common.f.

: GDB-PROP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" test/prop-test.f" GE-SRC-FILE+
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" prop-test" GE-EXPECT-OK
   s" self-test OK" s" prop-test self-test/run did not complete" GE-EXPECT-OUT-HAS
   s" PASS: prop-test soundness smoke (self-hosted in habu, in-process via evaluate)" type cr ;

: GDB-PTY ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   s" lib/errors.f" GE-ARG+
   s" lib/process.f" GE-ARG+
   s" test/proc-pty.f" GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" process/pty" GE-EXPECT-OK
   s" PASS: process/pty primitives" s" process/pty output" GE-EXPECT-OUT-HAS
   s" PASS: process/pty primitives" type cr ;

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
   s" --load" GE-ARG+
   s" src/arch/arm64/disasm.f" GE-ARG+
   s" tools/jitdump.f" GE-ARG+
   s" --" GE-ARG+
   s" : JITDUMP-SMOKE ( -- i64 ) 7 ;" GE-ARG+
   s" JITDUMP-SMOKE" GE-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" jitdump direct CLI" GE-EXPECT-OK
   s" ret" s" jitdump direct CLI output" GE-EXPECT-OUT-HAS
   s" PASS: jitdump direct CLI" type cr ;

: GDB-RUN ( -- )
   s" hb-gate-debug" GT-START
   GDB-PROP
   GDB-PTY
   GDB-PROFILER
   GDB-JITDUMP
   GT-CLEANUP
   s" PASS: native prop/debug gate phase" type cr ;

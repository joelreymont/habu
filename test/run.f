\ run.f - checked native default gate runner.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and lib/test-runner.f.

64 constant TR-USAGE-RC
600000 constant TR-TIMEOUT-MS

: TR-USAGE ( -- )
   s" usage: bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f" TR-USAGE-RC die ;

: TR-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: TR-CHECK-ARGS ( -- )
   SCRIPT-ARGC 0= if exit then
   SCRIPT-ARGC 1 = s" full" TR-ARG0= and if
      s" test/run.f full retired; the native gate is test/run.f" TR-USAGE-RC die
   then
   TR-USAGE ;

: TR-START ( -- )
   GT-RESET
   CLEANUP-RESET
   s" HB_TMP" GETENV dup 0= if
      2drop
      s" hb-gate" TMPDIR-MKDIR GT-COPY-ROOT!
      GT-ROOT CLEANUP-TREE+
      exit
   then
   2dup MAKE-DIRS
   GT-COPY-ROOT! ;

: TR-FAIL ( ptr u8 n -- ) {: label:ptr labelu :}
   s" FAIL: " type label labelu type cr
   GT-CLEANUP
   label labelu 1 die ;

: TR-BASE ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" GT-ROOT PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" lib/test-runner.f" PROC-ARGV+ ;

: TR-FLUSH-CAPTURE-LINES ( -- )
   1 GT-OUT-BUF PROC-OUT-LEN GT-FLUSH-LINES-FD
   2 GT-ERR-BUF PROC-ERR-LEN GT-FLUSH-LINES-FD ;

: TR-FLUSH-CAPTURE-FINAL ( -- )
   1 GT-OUT-BUF PROC-OUT-LEN GT-FLUSH-REMAINDER-FD
   2 GT-ERR-BUF PROC-ERR-LEN GT-FLUSH-REMAINDER-FD ;

: TR-SPAWN-CAPTURE ( -- )
   s" bin/hb" PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   TR-TIMEOUT-MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   s" bin/hb" PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE ;

: TR-RUN-CAPTURE-LOOP ( ptr u8 n -- ) {: label:ptr labelu :}
   begin PROC-CAPTURE-DONE? 0= while
      GT-PROGRESS-SLICE-MS PROC-POLL-CAPTURE-OUTCOME dup 0= if
         drop
         PROC-REMAINING-MS 0 <= if
            PROC-REAP-CAPTURE-TIMEOUT
            TR-FLUSH-CAPTURE-FINAL
            exit
         then
         label labelu GT-PROGRESS-WAIT
      else
         drop
         GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP PROC-DRAIN-READY
         TR-FLUSH-CAPTURE-LINES
         label labelu GT-PROGRESS-WAIT
      then
   repeat
   PROC-REAP-CAPTURE
   TR-FLUSH-CAPTURE-FINAL ;

: TR-PHASE-OK? ( -- bool )
   PROC-OUTCOME-KIND @ PROC-OUTCOME-EXIT =
   PROC-OUTCOME-CODE @ 0= and ;

: TR-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   TR-SPAWN-CAPTURE
   label labelu TR-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   TR-PHASE-OK? 0= if label labelu TR-FAIL then
   label labelu GT-PROGRESS-PASS ;

: TR-COMMON ( -- )
   s" test/gate-common.f" PROC-ARGV+ ;

: TR-BUILD-COMMON ( -- )
   TR-COMMON
   s" test/gate-build-common.f" PROC-ARGV+ ;

: TR-STDLIB ( -- )
   TR-BASE
   s" test/gate-stdlib.f" PROC-ARGV+
   s" native lint/stdlib gate phase" TR-RUN ;

: TR-ENGINE ( -- )
   TR-BASE
   TR-COMMON
   s" lib/build.f" PROC-ARGV+
   s" tools/build-fixpoint.f" PROC-ARGV+
   s" test/gate-engine.f" PROC-ARGV+
   s" native engine gate phase" TR-RUN ;

: TR-EXPECT-HB ( -- )
   s" bin/hb" EXECUTABLE? 0= if s" bin/hb not produced executable" TR-FAIL then ;

: TR-DICTIONARY ( -- )
   TR-BASE
   TR-COMMON
   s" test/gate-dictionary.f" PROC-ARGV+
   s" native dictionary/checker gate phase" TR-RUN ;

: TR-DIAGNOSTICS ( -- )
   TR-BASE
   TR-COMMON
   s" test/gate-diagnostics.f" PROC-ARGV+
   s" native checker diagnostics gate phase" TR-RUN ;

: TR-DEBUG ( -- )
   TR-BASE
   TR-COMMON
   s" lib/codesign.f" PROC-ARGV+
   s" test/gate-debug.f" PROC-ARGV+
   s" native prop/snapshot/debug gate phase" TR-RUN ;

: TR-AOT-POSITIVE ( -- )
   TR-BASE
   TR-BUILD-COMMON
   s" test/gate-aot-positive.f" PROC-ARGV+
   s" native hb-build AOT positive gate phase" TR-RUN ;

: TR-AOT-NEGATIVE ( -- )
   TR-BASE
   TR-BUILD-COMMON
   s" test/gate-aot-negative.f" PROC-ARGV+
   s" native hb-build AOT negative gate phase" TR-RUN ;

: TR-HB-BUILD-REPL ( -- )
   TR-BASE
   TR-BUILD-COMMON
   s" test/gate-hb-build-repl.f" PROC-ARGV+
   s" native hb-build REPL gate phase" TR-RUN ;

: TR-MAIN ( -- )
   TR-CHECK-ARGS
   TR-START
   TR-STDLIB
   TR-ENGINE
   TR-EXPECT-HB
   TR-DICTIONARY
   TR-DIAGNOSTICS
   TR-DEBUG
   TR-AOT-POSITIVE
   TR-AOT-NEGATIVE
   TR-HB-BUILD-REPL
   GT-CLEANUP
   s" PASS: native gate (fixpoint + engine suite + checked hb + repl + hb-build)" type cr ;

TR-MAIN

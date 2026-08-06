\ gate-debug.f - checked runner for prop/debug gate checks.
\
\ Load after test/gate-common.f.

require tools/jitdump-core.f

using GATE

: GDB-PROP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" test/prop-test.f" GE-SRC-FILE+
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" prop-test" GE-EXPECT-OK
   s" self-test OK" s" prop-test self-test/run did not complete" GE-EXPECT-OUT-HAS
   s" canary self-test OK" s" prop-test canary provenance teeth did not run" GE-EXPECT-OUT-HAS
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

\ --- profiler sample-accounting + counter-band regressions (dots
\ habu-make-profiler-sample-4df2965e + habu-bound-profiler-counter-235c5f48) ---
\ The auto-dump prints one "name count" (or "(other) count") line per hot bucket;
\ the total of those counts must equal the sample limit exactly. A long busy word
\ whose loop body inlines its primitives keeps every interrupted pc inside its own
\ code range, so the samples land in one deterministic bucket.

variable GDB-ACC
variable GDB-LS

: GDB-LAST-SP ( ptr u8 n -- n )   \ index of the last space byte in a line, -1 if none
   {: a:ptr u:n :}
   -1 0 begin dup u < while
      dup a + c@ STR-SPACE = if dup rot drop then
      1+
   repeat drop ;

: GDB-LINE-VAL ( ptr u8 n -- n )   \ trailing space-delimited integer of a line, 0 if none
   {: a:ptr u:n :}
   u 0= if 0 exit then
   a u GDB-LAST-SP {: sp:n :}
   sp 0 < if 0 exit then
   sp 1+ {: t:n :}
   t u >= if 0 exit then
   a t + u t - STR>NUMBER? MATCH option
     none OF 0 ENDOF
     some OF ENDOF
   ;MATCH ;

: GDB-SUM-COUNTS ( ptr u8 n -- n )   \ sum the trailing integer of every LF-split line
   {: a:ptr u:n :}
   0 GDB-ACC !  0 GDB-LS !
   0 begin dup u < while
      dup a + c@ STR-LF = if
         a GDB-LS @ + over GDB-LS @ - GDB-LINE-VAL GDB-ACC @ + GDB-ACC !
         dup 1+ GDB-LS !
      then
      1+
   repeat drop
   GDB-LS @ u < if
      a GDB-LS @ + u GDB-LS @ - GDB-LINE-VAL GDB-ACC @ + GDB-ACC !
   then
   GDB-ACC @ ;

: GDB-PROF-SRC ( n -- )   \ emit a busy word + "<n> prof-on GDB-BUSY" into GE-SRC
   GE-SRC-RESET
   s" : GDB-BUSY ( -- ) 80000000 begin 1- dup dup * drop dup 0= until drop ;" GE-SRC-LINE
   GE-SRC-U+
   s"  prof-on GDB-BUSY" GE-SRC-LINE ;

: GDB-PROF-RUN ( -- )   \ run the accumulated GE-SRC through the candidate over stdin
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN ;

: GDB-PROFILER-BAND ( -- )   \ counter band must hold >= DICT-CAP counters (bound dot)
   GE-HB-RESET
   GE-SRC-RESET
   s" PROF-CNT-BYTES DICT-CAP cells >= ." GE-SRC-LINE
   GDB-PROF-RUN
   s" profiler counter band capacity probe" GE-EXPECT-OK
   GT-OUT$ s" -1" STARTS-WITH? 0= if
      s" profiler counter band holds fewer than DICT-CAP counters" GE-FAIL
   then
   s" PASS: profiler counter band covers every DICT-CAP slot" type cr ;

: GDB-PROFILER-LIMIT1 ( -- )   \ limit 1 must attribute (not drop) the only sample
   GE-HB-RESET
   1 GDB-PROF-SRC
   GDB-PROF-RUN
   99 s" profiler limit 1 auto-dump exit(99)" GE-EXPECT-RC
   GT-OUT$ nip 0= if
      s" profiler limit 1 dropped the only sample: empty dump" GE-FAIL
   then
   s" PASS: profiler limit 1 attributes the interrupted sample" type cr ;

: GDB-PROFILER-EXACT ( n -- )   \ sum(counters)+other == limit at auto-dump
   {: lim:n :}
   GE-HB-RESET
   lim GDB-PROF-SRC
   GDB-PROF-RUN
   99 s" profiler exact-totals auto-dump exit(99)" GE-EXPECT-RC
   GT-OUT$ GDB-SUM-COUNTS lim <> if
      s" profiler exact-totals: sum(counters)+other != limit" GE-FAIL
   then ;

: GDB-PROFILER-EXACT-ALL ( -- )
   1 GDB-PROFILER-EXACT
   2 GDB-PROFILER-EXACT
   5 GDB-PROFILER-EXACT
   s" PASS: profiler sample totals are exact (sum == limit)" type cr ;

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
   GDB-PROFILER-BAND
   GDB-PROFILER-LIMIT1
   GDB-PROFILER-EXACT-ALL
   GDB-JITDUMP
   GT-CLEANUP
   s" PASS: native prop/debug gate phase" type cr ;

;using

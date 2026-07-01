\ run-worker.f - phase-owned resident worker dispatcher.
\
\ Loaded inside a forked worker after test/run-resident.f stores TR-RESIDENT-ID.

: TRW-LOAD ( ptr u8 n -- )
   included ;

: TRW-RUN ( -- )
   TR-RESIDENT-ID @ case
      2 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      3 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      5 of s" test/run-worker-engine.f" TRW-LOAD endof
      6 of s" test/run-worker-debug.f" TRW-LOAD endof
      7 of s" test/run-worker-aot.f" TRW-LOAD endof
      8 of s" test/run-worker-aot-neg.f" TRW-LOAD endof
      9 of s" test/run-worker-engine.f" TRW-LOAD endof
      10 of s" test/run-worker-diag.f" TRW-LOAD endof
      11 of s" test/run-worker-diag.f" TRW-LOAD endof
      12 of s" test/run-worker-diag-all-strict.f" TRW-LOAD endof
      13 of s" test/run-worker-diag.f" TRW-LOAD endof
      14 of s" test/run-worker-dict.f" TRW-LOAD endof
      16 of s" test/run-worker-engine.f" TRW-LOAD endof
      17 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      18 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      21 of s" test/run-worker-engine.f" TRW-LOAD endof
      22 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      23 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      24 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      25 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      26 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      27 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      28 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      29 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      31 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      32 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      33 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      34 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      35 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      36 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      37 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      38 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      39 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      40 of s" test/run-worker-stdlib.f" TRW-LOAD endof
      E-TBL-BOUNDS throw
   endcase ;

TRW-RUN

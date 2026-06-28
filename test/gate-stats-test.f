\ gate-stats-test.f - focused coverage for gate-stats.f.
\
\ Load after lib/test.f and test/gate-stats.f.

create GST-ROOT-BUF FS-PATH-CAP allot
variable GST-ROOT-U

: GST-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GST-ROOT-BUF u BYTE-COPY
   u GST-ROOT-U ! ;

: GST-ROOT$ ( -- ptr u8 n )
   GST-ROOT-BUF GST-ROOT-U @ ;

: GST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-gate-stats" TMPDIR-MKDIR GST-COPY!
   GST-ROOT$ CLEANUP-TREE+
   GST-ROOT$ GS-ROOT! ;

: GST-WRITE-EVENTS ( -- )
   s" top-phase-spawn" GS-EVENT
   s" top-capture-spawn" GS-EVENT
   s" under-phase-spawn" GS-EVENT
   s" under-env" GS-EVENT
   s" runner-phase-spawn" GS-EVENT
   s" gate-runner-build" GS-EVENT
   s" inner-hb-spawn" GS-EVENT
   s" inner-hb-stdin" GS-EVENT
   s" inner-hb-stdin" GS-EVENT
   s" boundary-test" GS-EVENT
   s" boundary-test" GS-EVENT
   s" warm-cache-hit" GS-EVENT
   s" warm-cache-miss" GS-EVENT
   s" warm-build" GS-EVENT
   s" warm-sig-export" GS-EVENT
   s" warm-snapshot" GS-EVENT
   s" maker-cache-hit" GS-EVENT
   s" maker-cache-miss" GS-EVENT
   s" maker-build" GS-EVENT
   s" maker-run" GS-EVENT
   s" candidate-build" GS-EVENT
   s" candidate-cache-hit" GS-EVENT
   s" candidate-cache-miss" GS-EVENT
   s" candidate-cache-install" GS-EVENT
   s" helper-spawn" GS-EVENT ;

: GST-SCAN ( -- )
   GS-READ
   GS-SCAN ;

: GST-EXPECT-COUNTS ( -- )
   GS-TOP-PHASE @ 1 T=
   GS-TOP-CAPTURE @ 1 T=
   GS-UNDER-PHASE @ 1 T=
   GS-UNDER-ENV @ 1 T=
   GS-RUNNER-PHASE @ 1 T=
   GS-RUNNER-BUILD @ 1 T=
   GS-INNER-HB @ 1 T=
   GS-INNER-HB-STDIN @ 2 T=
   GS-BOUNDARY @ 2 T=
   GS-WARM-HIT @ 1 T=
   GS-WARM-MISS @ 1 T=
   GS-WARM-BUILD @ 1 T=
   GS-WARM-SIG @ 1 T=
   GS-WARM-SNAP @ 1 T=
   GS-MAKER-HIT @ 1 T=
   GS-MAKER-MISS @ 1 T=
   GS-MAKER-BUILD @ 1 T=
   GS-MAKER-RUN @ 1 T=
   GS-CANDIDATE @ 1 T=
   GS-CANDIDATE-HIT @ 1 T=
   GS-CANDIDATE-MISS @ 1 T=
   GS-CANDIDATE-INSTALL @ 1 T=
   GS-HELPER-SPAWN @ 1 T= ;

: GST-TEST-SCAN ( -- )
   GST-PREPARE
   GS-PATH$ FILE? TTRUE
   GST-WRITE-EVENTS
   GST-SCAN
   GST-EXPECT-COUNTS ;

: GST-MAIN ( -- )
   T-RESET
   GST-TEST-SCAN
   CLEANUP-RUN
   GST-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" gate-stats-test: ok" type cr ;

GST-MAIN

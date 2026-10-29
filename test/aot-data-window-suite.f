\ aot-data-window-suite.f - what the seed does with a capture window's DATA.
\ Each case boots a built engine on a batch program; the seed runs at the end of
\ the engine prefix before any input, so a boot-run report, a boot die and its
\ exit code reach the output on every host (test/aot-wide-format-lib.f says
\ why).
\ Registered as `SUITE aot-data-window` in test/gate-stdlib-cases.f.

require src/lib/string.f
require src/lib/test.f
require src/lib/fmt.f
require src/lib/test/outcome.f
require test/aot-wide-format-lib.f

package AOT-WIDE-FORMAT

create SPAN-BUF 32 allot   variable SPAN-U

\ The exit is asserted under the caller's label, so a hang (at the deadline) or
\ a fault fails that case instead of escaping the probe uncaught.
: BOOT-DIES ( n -- ) {: want:n :}
   PROC-ARGV-RESET
   HB$ >LEN  BATCH-PROGRAM$ >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   BATCH-PROGRAM$ OUT outu LEN>N ERR erru LEN>N oc want T-OUTCOME-EXITED= ;

\ DATA-SIZE is per target, so the forged span is computed, not a host literal.
: SPAN$ ( -- ptr u8 n )
   SB-RESET  DATA-SIZE 2 * FMT:SB-U  SB$ {: a:ptr u:n :}
   a SPAN-BUF u BYTE-COPY  u SPAN-U !
   SPAN-BUF SPAN-U @ ;

\ EM-AOT-RELOC-DATA copies the captured window instead of reserving it zeroed.
\ The HABU_AOT_BAKE=1 window's boot-run word reads an initialised cell through a
\ DATA address literal, so it reports the value only if the content travelled
\ and the literal was rebased onto the seeded DP; a zeroed reserve reports 0.
: PROBE-CONTENT ( -- )
   SETUP
   s" HABU_AOT_BAKE" BUILD-MODE
   s" a window holding an initialised cell builds cleanly" REQUIRE-BUILD
   s" the content variant image exists after the build" T-LABEL
   HB$ FS:EXISTS? TTRUE
   BATCH-OK
   s" the content variant runs a batch program with its real span" T-LABEL
   RC @ 0 T=
   s" and computes with it" T-LABEL
   ANSWERED
   \ $5A5AC0DEC0DE5A5A, set in test/aot-wid-build.f BAKE-FIXTURE-LINES.
   s" the initialised cell reports its own value at boot, not zero" T-LABEL
   s" awb-cell=" s" 6510728274268543578" REPORT= ;

\ EM-AOT-RELOC-DATA refuses a baked span past the DATA region's headroom (always
\ under DATA-SIZE) with exit 82 (ENGINE-ERROR:AOT-SEED). The forge is
\ PROBE-CONTENT's window written again with its span at 2*DATA-SIZE; the content
\ case is the same window with its real span, so the guard refuses only the
\ forge.
: PROBE-SPAN ( -- )
   BUILD-OPEN
   s" HABU_AOT_REWRITE" s" 1" KNOB+
   s" HABU_AOT_SPAN" SPAN$ KNOB+
   BUILD-RUN
   s" the same window writes again with its span forged past the DATA region" REQUIRE-BUILD
   s" the forged span dies at the seed with ENGINE-ERROR:AOT-SEED" T-LABEL
   ENGINE-ERROR:AOT-SEED BOOT-DIES
   s" ... naming the guard, before any input is read" T-LABEL
   ERR$ S\" hb: AOT data span out of range\n" T$=
   OUT$ s" batch-answer=" CONTAINS? 0= TTRUE ;

\ The capture zeroes every declared address cell (a building-host code address)
\ and the seed writes the engine's defer-unset xt back. HABU_AOT_TRAP=1 calls an
\ uninstalled `defer` from the boot-run, which must die with EXEC-VECTOR-RC; a
\ zero there was measured as SIGSEGV with no diagnostic.
: PROBE-TRAP ( -- )
   SETUP
   s" HABU_AOT_TRAP" BUILD-MODE
   s" a window holding an uninstalled defer builds cleanly" REQUIRE-BUILD
   s" the trap variant image exists after the build" T-LABEL
   HB$ FS:EXISTS? TTRUE
   s" the boot-run's call through the re-trapped cell exits EXEC-VECTOR-RC, not a fault" T-LABEL
   EXEC-VECTOR-RC BOOT-DIES
   s" ... naming the unset vector, before any input is read" T-LABEL
   ERR$ s" defer: unset execution vector" CONTAINS? TTRUE
   OUT$ s" batch-answer=" CONTAINS? 0= TTRUE ;

: BODY ( -- )
   PROBE-CONTENT
   PROBE-SPAN
   PROBE-TRAP ;

public

: RUN ( -- )
   [: BODY ;] RUN-PROBES
   s" aot-data-window: ok" type cr ;

;package

AOT-WIDE-FORMAT:RUN

\ aot-data-window-suite.f - what the seed does with a capture window's DATA
\ (dots habu-guard-aot-data-49de2ee6 and habu-bake-the-aot-7ececce8).
\
\ Every case boots a built engine on an ordinary batch program and reads what it
\ wrote. The seed runs at the end of the engine prefix on every boot, before any
\ input is read, so a boot-run report, a boot die and its exit code all reach a
\ batch boot's output on every host (test/aot-wide-format-lib.f says why).
\
\ THE WINDOW'S CONTENT. EM-AOT-RELOC-DATA copies the captured DATA window instead
\ of reserving it zeroed. The HABU_AOT_BAKE=1 window holds an INITIALISED cell and
\ a word that reads it through a DATA address literal, run from the boot-run list,
\ so its report carries the cell's value only if the content travelled AND the
\ literal was rebased onto the seeded DP. The zeroed reserve reports 0.
\
\ THE SPAN GUARD. EM-AOT-RELOC-DATA refuses a baked span past the DATA region's
\ headroom, naming it and exiting 82 (ENGINE-ERROR:AOT-SEED); without the bound
\ the oversized span is accepted in silence. The content case's window, written
\ again with its baked span forged to 2*DATA-SIZE - past any headroom, which is
\ always under DATA-SIZE - must die at the seed. The content case is that same
\ window with its real span, booting, so the guard neither passes an oversized
\ span nor refuses a legal one.
\
\ THE DECLARED CELL. The capture zeroes every declared address cell, because the
\ value there is a code address in the BUILDING host, and the seed writes the
\ engine's own defer-unset xt back. The HABU_AOT_TRAP=1 window holds a `defer`
\ nothing installs and a boot-run word that calls it, so the boot must die
\ "defer: unset execution vector" with EXEC-VECTOR-RC. Zero would have been a
\ jump to address 0 - measured as SIGSEGV, no diagnostic.
\
\ Cost: two captures and one more write. Registered as `SUITE aot-data-window`
\ in test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-data-window-suite.f

require lib/test.f
require lib/fmt.f
require lib/test/outcome.f
require test/aot-wide-format-lib.f

package AOT-WIDE-FORMAT

create SPAN-BUF 32 allot   variable SPAN-U

\ A boot that must die at the seed or in the boot-run. Its exit is asserted under
\ the caller's label, so a boot that hangs or faults instead fails that case -
\ at the deadline for a hang - rather than escaping the probe uncaught.
: BOOT-DIES ( n -- ) {: want:n :}
   PROC-ARGV-RESET
   HB$ >LEN  BATCH-PROGRAM$ >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   want T-OUTCOME-EXITED=
   LEN>N ERR-U !  LEN>N OUT-U ! ;

\ 2*DATA-SIZE in decimal. DATA-SIZE is per target, so the forged span is
\ computed from it rather than pinned to one host's literal.
: SPAN$ ( -- ptr u8 n )
   SB-RESET  DATA-SIZE 2 * FMT:SB-U  SB$ {: a:ptr u:n :}
   a SPAN-BUF u BYTE-COPY  u SPAN-U !
   SPAN-BUF SPAN-U @ ;

: PROBE-CONTENT ( -- )
   SETUP
   s" HABU_AOT_BAKE" BUILD-MODE
   s" a window holding an initialised cell builds cleanly" REQUIRE-BUILD
   s" the content variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   BATCH-OK
   s" the content variant runs a batch program with its real span" T-LABEL
   RC @ 0 T=
   s" and computes with it" T-LABEL
   ANSWERED
   \ $5A5AC0DEC0DE5A5A, set in test/aot-wid-build.f BAKE-FIXTURE-LINES.
   s" the initialised cell reports its own value at boot, not zero" T-LABEL
   s" awb-cell=" s" 6510728274268543578" REPORT= ;

\ Runs in PROBE-CONTENT's tree: the forge is that window written again.
: PROBE-SPAN ( -- )
   BUILD-OPEN
   s" HABU_AOT_REWRITE" s" 1" KNOB+
   s" HABU_AOT_SPAN" SPAN$ KNOB+
   BUILD-RUN
   s" the same window writes again with its span forged past the DATA region" REQUIRE-BUILD
   s" the forged span dies at the seed with ENGINE-ERROR:AOT-SEED" T-LABEL
   ENGINE-ERROR:AOT-SEED BOOT-DIES
   s" ... naming the guard, before any input is read" T-LABEL
   ERR$ s" hb: AOT data span out of range" CONTAINS? TTRUE
   OUT$ s" batch-answer=" CONTAINS? 0= TTRUE ;

: PROBE-TRAP ( -- )
   SETUP
   s" HABU_AOT_TRAP" BUILD-MODE
   s" a window holding an uninstalled defer builds cleanly" REQUIRE-BUILD
   s" the trap variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   s" the boot-run's call through the re-trapped cell exits EXEC-VECTOR-RC, not a fault" T-LABEL
   EXEC-VECTOR-RC BOOT-DIES
   s" ... naming the unset vector, before any input is read" T-LABEL
   ERR$ s" defer: unset execution vector" CONTAINS? TTRUE
   OUT$ s" batch-answer=" CONTAINS? 0= TTRUE ;

: BODY ( -- )
   PROBE-CONTENT
   PROBE-SPAN
   PROBE-TRAP ;

\ Public so the driver below runs it with the package closed.
public

: RUN ( -- )
   [: BODY ;] RUN-PROBES
   s" aot-data-window: ok" type cr ;

;package

AOT-WIDE-FORMAT:RUN

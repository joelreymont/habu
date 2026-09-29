\ aot-wide-format-suite.f - what the widened AOT capture format can now carry
\ (dot habu-widen-the-aot-089f5faf).
\
\ WHAT THIS LOCKS. Every offset in the baked ahead-of-time frame used to be a
\ u16: the call-site rows (blob-off, name-off), the DATA-site list, the CODE-site
\ list and the window's declared-address-cell list. Sixteen bits cannot name a
\ byte past 65535, so a captured window could never be larger than that whatever
\ the buffers said - and the capture died at AOT-BLOB-CAP with "blob exceeds
\ buffer" before it could get there. The compiler chain this seed exists to carry
\ measures 1.15 MB. With the fields widened to u32 and the buffers lifted to
\ match, a window beyond the old ceiling must capture, bake and boot.
\
\ HOW IT IS PROVEN, AND WHAT IS PROVEN WHERE. test/aot-wid-build.f is spawned in
\ a child process with HABU_AOT_BIG=1 and a private HB_TMP. That mode compiles a
\ thousand filler words and then, ABOVE them, the three things that have to
\ survive the crossing: a data cell, a callee too long for the inliner to copy,
\ and a reporter that calls the callee and prints the cell. The window is taken
\ in by the same widened re-capture the other window fixtures use - the real
\ AOT-CAPTURE:CAPTURE entry point, not a stand-in - and the builder then reads
\ its own capture tables and DIES unless all three of these clear the old
\ ceiling: the blob length, the highest call-site blob offset, and the highest
\ DATA-site blob offset. Those refusals are the assertions; the three lines this
\ suite matches are printed only on the far side of them, so a window that
\ stopped being big enough fails the build instead of quietly proving nothing.
\
\ THE SECOND THING THE WIDENING BUYS, and the second case here: a NAME that does
\ not fit its dictionary record. Past DNAME-INL the definer keeps the name out of
\ line and the record's [24] cell points at the bytes; the capture used to refuse
\ such a record by name ("rec has EXT name (uncompactable)"), and the compiler
\ chain has 45 of them. The HABU_AOT_EXT=1 mode puts one in the window and dies
\ unless the capture really produced an out-of-line record, so the case cannot
\ pass on a window whose names all shrank back under the limit.
\
\ The window words that reach a PREFIX word - a data word and a code literal -
\ are test/aot-wide-prefix-suite.f, a gate row of its own; both rows share the
\ fixture and the boot-half reader in test/aot-wide-format-lib.f.
\
\ Cost: two child engine builds; the big-window one is larger than the other
\ because the maker compiles the filler. Registered as
\ `TEST:SUITE aot-wide-format` in test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-wide-format-suite.f

require lib/test.f
require test/aot-wide-format-lib.f

package AOT-WIDE-FORMAT

: PROBE-BIG-WINDOW ( -- )
   SETUP
   s" HABU_AOT_BIG" BUILD-MODE
   s" a capture window past the old 64 KiB ceiling builds cleanly" REQUIRE-BUILD
   s" the captured blob passed 64 KiB" T-LABEL
   OUT$ s" aot-wid-build: big-blob " CONTAINS? TTRUE
   s" a call site was recorded above the old u16 offset ceiling" T-LABEL
   OUT$ s" aot-wid-build: big-site " CONTAINS? TTRUE
   s" a DATA site was recorded above the old u16 offset ceiling" T-LABEL
   OUT$ s" aot-wid-build: big-dsite " CONTAINS? TTRUE
   s" the over-64 KiB variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   BATCH-OK
   s" the over-64 KiB variant still runs a batch program" T-LABEL
   RC @ 0 T=
   s" and computes with it" T-LABEL
   ANSWERED
   \ THE ACCEPTANCE. The reporter was compiled ABOVE 100 KiB of filler, so its own
   \ blob offset, the DATA site it reads the cell through and the call site it
   \ reaches the callee through are all past 65535. It can only print the cell's
   \ magic if the seed patched a call site AND a DATA site at offsets no u16 field
   \ could have held. $5A5AB16B16B15A5A, set in test/aot-wid-build.f.
   s" the captured window past 64 KiB reports its magic at boot" T-LABEL
   s" awb-big=" s" 6510711284817812058" REPORT= ;

: PROBE-EXT-NAME ( -- )
   SETUP
   s" HABU_AOT_EXT" BUILD-MODE
   s" a window holding an out-of-line name builds cleanly" REQUIRE-BUILD
   s" the capture produced an out-of-line-named record" T-LABEL
   OUT$ s" aot-wid-build: ext-recs " CONTAINS? TTRUE
   s" the out-of-line-name variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   BATCH-OK
   s" the out-of-line-name variant still runs a batch program" T-LABEL
   RC @ 0 T=
   s" and computes with it" T-LABEL
   ANSWERED
   \ The boot-run resolves its entry words through LFIND, and for an EXT record
   \ LFIND compares the token against the bytes [24] points at - which the seed
   \ sets to the baked name pool. A wrong pointer finds nothing and the boot-run
   \ exits $52 with no report at all, so the printed magic is the pointer's proof.
   s" the out-of-line-named word is found by that name at boot" T-LABEL
   s" awb-ext=" s" 6510767442340633178" REPORT= ;

: BODY ( -- )
   PROBE-BIG-WINDOW
   PROBE-EXT-NAME ;

\ Public so the driver below runs it with the package closed.
public

: RUN ( -- )
   [: BODY ;] RUN-PROBES
   s" aot-wide-format: ok" type cr ;

;package

AOT-WIDE-FORMAT:RUN

\ aot-wide-prefix-suite.f - window words that reach a PREFIX word through the
\ widened AOT capture format (dots habu-aot-pre-window-0b01043c and
\ habu-widen-the-aot-089f5faf).
\
\ THE FIRST CASE, and the one that is an ELIMINATION rather than a carry (dot
\ habu-aot-pre-window-0b01043c). A window word that names a PREFIX data word used
\ to end the build: the prefix word's body is short, the engine's inliner copied
\ it, and the copy carried an address below the window's DATA span, which the
\ capture correctly refuses because no delta relates the metabuild host's prefix
\ band to the target's. Carrying such an address was measured and refuted, so the
\ engine now DECLINES to copy a body holding a chain the open window cannot
\ describe and emits its call instead - a call the capture records by name and the
\ seed resolves in the engine it is booting. The HABU_AOT_PREWIN=1 mode reads the
\ capture's own tables back over the fixture word's record and dies unless the body
\ is free of DATA sites and holds the call.
\
\ THE SECOND CASE, and the one the ruling's rider named for dot
\ habu-widen-the-aot-089f5faf: a pre-window CODE literal, which is also what puts
\ a NAMED code row on the bake-and-boot path.
\ `['] X` on a PREFIX word compiles its chain into the window word's OWN body, so
\ the decline that emptied the DATA class cannot reach it - there is no copy to
\ decline. The capture recognises it as a call target that is not a BL and writes a
\ name-keyed row instead, and the seed resolves that name in the engine it is
\ booting. The HABU_AOT_XTLIT=1 mode asserts, over the fixture word's own captured
\ record, exactly one such row inside its body naming the prefix word and no
\ rebased code site there. On the base this build DIES named.
\ It is the ONLY case the row kind needs. A row can only ever name a word the
\ window does not contain (aot-capture.f ACAP-OUT-CHAIN returns early for a value
\ inside the blob span, and every captured record's entry is inside it), so a
\ fixture that doctors a row onto a WINDOW word tests a lookup the classifier
\ cannot produce - which is what the deleted HABU_AOT_XTSITE mode did.
\
\ The window past 64 KiB and the out-of-line name are test/aot-wide-format-suite.f,
\ a gate row of its own; both rows share the fixture and the boot-half reader in
\ test/aot-wide-format-lib.f.
\
\ Cost: two child engine builds. Registered as `TEST:SUITE aot-wide-prefix` in
\ test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-wide-prefix-suite.f

require lib/test.f
require test/aot-wide-format-lib.f

package AOT-WIDE-FORMAT

\ A window word that names a PREFIX data word (dot habu-aot-pre-window-0b01043c).
\ The prefix's `create` sits below the window's DATA span, so the address its body
\ pushes is one the window cannot describe, and the body is short enough that the
\ engine's compile-mode inliner used to COPY it into the caller - which is how the
\ address got in. On the unfixed base this exact mode dies at build time,
\ "aot-capture: recorded address site ... in neither the window's DATA span nor its
\ code span", exit 74, with no image produced. The engine now declines that copy
\ and emits the call instead.
\
\ WHAT THE TWO PRINTED LINES MEAN. The builder reads its own capture tables over
\ the fixture word's captured dict record, found by name: prewin-dsites is how many
\ DATA relocation sites lie inside that word's blob span (must be zero - the chain
\ is gone) and prewin-calls how many call sites inside it name the prefix word
\ (must not be zero - the BL is there and carries the name the seed resolves). The
\ builder DIES rather than print either line if its half fails, so matching them is
\ matching assertions that already passed; asserting both is what stops a fixture
\ that quietly stopped naming a prefix word from looking like a pass.
: PROBE-PREWINDOW ( -- )
   SETUP
   s" HABU_AOT_PREWIN" BUILD-MODE
   s" a window word naming a prefix data word builds cleanly" REQUIRE-BUILD
   s" its body carries no DATA relocation site" T-LABEL
   OUT$ s" aot-wid-build: prewin-dsites 0" CONTAINS? TTRUE
   s" its body calls the prefix word by name instead" T-LABEL
   OUT$ s" aot-wid-build: prewin-calls 1" CONTAINS? TTRUE
   s" the pre-window variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   BATCH-OK
   s" the pre-window variant still runs a batch program" T-LABEL
   RC @ 0 T=
   s" and computes with it" T-LABEL
   ANSWERED
   \ HH0's first cell is the SHA-256 seed constant $6a09e667, initialised by
   \ src/core/sha256.f in the cold prefix. The window word can only read it if the
   \ seed resolved HH0's name in THIS engine and patched the call the decline
   \ emitted; an unrelocated read gives zero and a wrong one crashes.
   s" the relocated call reaches the prefix word's own cell at boot" T-LABEL
   s" awb-pre=" s" 1779033703" REPORT= ;

\ THE PRE-WINDOW CODE LITERAL, both halves (dot habu-widen-the-aot-089f5faf).
\ The build half is the structural one and lives in the builder: over the fixture
\ word's own captured record, exactly one named code row inside its body, naming
\ HH0, and no rebased code site there.
\
\ THE BOOT HALF COMPARES TWO TICKS OF ONE WORD. The reporter ticks HH0 from INSIDE
\ the window, where the value can only be what the seed wrote; the probe program
\ ticks it from OUTSIDE, in code this engine compiles at boot. Requiring the two to
\ be equal needs no fixed number, which is what makes it survive ASLR - and neither
\ wrong answer can produce it, because the building host's address is not this
\ engine's and the zero the capture leaves in the lanes is not either. The two
\ ticks are read as spans and compared, so a report that stopped printing digits
\ cannot match a probe that also stopped.
: PROBE-XTLIT ( -- )
   SETUP
   s" HABU_AOT_XTLIT" BUILD-MODE
   s" a window holding a code literal for a prefix word builds cleanly" REQUIRE-BUILD
   s" the capture made a named code row for it" T-LABEL
   OUT$ s" aot-wid-build: xtlit " CONTAINS? TTRUE
   s" and left no rebased code site in that body" T-LABEL
   OUT$ s" aot-wid-build: xtlit-csites 0" CONTAINS? TTRUE
   s" the code-literal variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   S\" TRUSTED: XLP>BITS ( [ -- ptr a ] -- n ) ;\n: XLP ( -- ) s\" xl-live=\" type ['] HH0 XLP>BITS . cr ; XLP" BATCH-RUN
   s" the code-literal variant still runs a batch program" T-LABEL
   RC @ 0 T=
   s" the baked literal is this engine's own entry for the word it names" T-LABEL
   s" awb-xl=" REPORT$ {: a:ptr u:n :}
   u 0 T<>
   a u  s" xl-live=" REPORT$  STR= TTRUE ;

: BODY ( -- )
   PROBE-XTLIT
   PROBE-PREWINDOW ;

\ Public so the driver below runs it with the package closed.
public

: RUN ( -- )
   [: BODY ;] RUN-PROBES
   s" aot-wide-prefix: ok" type cr ;

;package

AOT-WIDE-FORMAT:RUN

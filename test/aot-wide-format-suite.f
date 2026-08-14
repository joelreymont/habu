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
\ match, a window several times the old ceiling must capture, bake and boot.
\
\ HOW IT IS PROVEN, AND WHAT IS PROVEN WHERE. test/aot-wid-build.f is spawned in
\ a child process with HABU_AOT_BIG=1 and a private HB_TMP. That mode compiles a
\ few hundred filler words and then, ABOVE them, the three things that have to
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
\ THE THIRD THING, and the one whose producer is still ahead of it: a NAMED code
\ site. A code-address literal whose value is a word's entry can be resolved by
\ name at boot instead of rebased - which is the only correct answer for a
\ literal naming a PRE-WINDOW word, whose address the window cannot describe (dot
\ habu-aot-pre-window-0b01043c owns deciding which sites those are). The row and
\ its boot arm ship with the rest of the format so it migrates once. The
\ HABU_AOT_XTSITE=1 mode makes such a row out of a real capture, through the
\ capture's own writer, and dies unless exactly one was made.
\
\ THE FOURTH CASE, and the one that is an ELIMINATION rather than a carry (dot
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
\ NOT COVERED HERE, and covered rather than faked: that the engines so built
\ actually BOOT - the over-64 KiB one reporting its magic, the out-of-line-named
\ one being FOUND by its long name, the named code site resolving to the word it
\ names rather than the one the chain pointed at, and the pre-window one reading
\ the prefix word's initialised cell through the relocated call. The AOT seed is
\ armed at the interactive REPL entry and nowhere else, so only a PTY boot can
\ observe any of them; that half lives in test/aot-data-span-forge.f beside the
\ other seed-pass boot regressions, and runs on Linux hosts, which are the ones
\ this tree's PTY helper supports. What this suite adds on every host is that the
\ capture and the bake succeed at all - the half that used to be impossible in all
\ four cases.
\
\ Cost: four child engine builds; the big-window one is larger than the others
\ because the maker compiles the filler. Registered as
\ `TEST:SUITE aot-wide-format` in test/gate-stdlib-cases.f. Run standalone:
\   bin/hb --load test/aot-wide-format-suite.f

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package AOT-WIDE-FORMAT

$8000 constant CAP
240000 constant BUILD-TIMEOUT-MS
30000  constant PROBE-TIMEOUT-MS

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create HB-BUF FS-PATH-CAP allot      variable HB-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: HB$ ( -- ptr u8 n )   HB-BUF HB-U @ ;
: PLAIN$ ( -- ptr u8 n ) s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

\ One tree per build, each registered for cleanup, so "the image exists" is a
\ statement about the build that just ran and never about a leftover.
: SETUP ( -- )
   s" habu-aot-wide" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-pwid" HB-BUF JOIN-PATH HB-U ! ;

: BUILDER-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+ ;

: BUILD-MODE ( ptr u8 n -- ) {: k:ptr ku:n :}
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   k ku >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   BUILDER-ARGV
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

\ The built engine still runs an ordinary batch program. A batch boot never arms
\ the seed, so this says nothing about the blob; it says the image the widened
\ bake produced is a working engine, which is what makes the PTY sibling's
\ report attributable to the seed rather than to a broken build.
: BATCH-OK ( -- )
   PROC-ARGV-RESET
   HB$ >LEN  s" 7 6 * . cr" >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: REQUIRE-BUILD ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   RC @ 0 T=
   RC @ 0 <> if s" aot-wide-format: builder stderr:" type cr  ERR$ type cr  RC @ throw then ;

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
   OUT$ s" 42" CONTAINS? TTRUE ;

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
   OUT$ s" 42" CONTAINS? TTRUE ;

: PROBE-XTSITE ( -- )
   SETUP
   s" HABU_AOT_XTSITE" BUILD-MODE
   s" a window carrying a named code site builds cleanly" REQUIRE-BUILD
   s" the capture turned one code site into a named row" T-LABEL
   OUT$ s" aot-wid-build: xtsite " CONTAINS? TTRUE
   s" the named-code-site variant image exists after the build" T-LABEL
   HB$ EXISTS? TTRUE
   BATCH-OK
   s" the named-code-site variant still runs a batch program" T-LABEL
   RC @ 0 T=
   s" and computes with it" T-LABEL
   OUT$ s" 42" CONTAINS? TTRUE ;

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
   OUT$ s" 42" CONTAINS? TTRUE ;

: BODY ( -- )
   PROBE-BIG-WINDOW
   PROBE-EXT-NAME
   PROBE-XTSITE
   PROBE-PREWINDOW ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-wide-format: ok" type cr ;

;package

AOT-WIDE-FORMAT:RUN

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
\ THE FIFTH CASE, and the one the ruling's rider named for this dot: a pre-window
\ CODE literal. `['] X` on a PREFIX word compiles its chain into the window word's
\ OWN body, so the decline that emptied the DATA class cannot reach it - there is
\ no copy to decline. The capture now recognises it as a call target that is not a
\ BL and writes a name-keyed row, which is the row kind the third case ships the
\ format for. The HABU_AOT_XTLIT=1 mode asserts, over the fixture word's own
\ captured record, exactly one such row inside its body naming the prefix word and
\ no rebased code site there. On the base this build DIES named.
\
\ AND THE BOOT HALF IS HERE TOO, ON EVERY HOST. That used to be impossible: the
\ AOT seed was armed at the interactive REPL entry and nowhere else, so only a PTY
\ could observe a captured word and the boot half of all four cases lived in
\ test/aot-data-span-forge.f, which runs on Linux hosts alone - it prints
\ "PTY boot cases run on linux only; skipped" everywhere else, so on a macOS host
\ NOTHING was asserting that any of this boots. Since dot
\ habu-decide-arm-the-5234727b the seed runs at the end of the engine prefix on
\ EVERY boot, so the boot-run entry words report into an ordinary batch boot's
\ stdout - and this suite was already spawning each built engine on a batch
\ program and reading that stdout, to check the image is a working engine at all.
\ The reports were sitting unread in the text it already had. Asserting them is
\ what makes "capture and boot a blob larger than 64 KiB" a claim this suite
\ proves rather than one it hands to a host it may not be running on. The PTY
\ sibling keeps its own copies; what only IT can still say is what the engine does
\ when it is entered INTERACTIVELY, since the entry words ask TTY? themselves.
\
\ Cost: five child engine builds; the big-window one is larger than the others
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

\ Boot the built engine on an ordinary batch program and capture what it wrote.
\ TWO things arrive in that stdout and they are different claims. The program's own
\ answer says the image the widened bake produced is a working engine. The lines
\ AHEAD of it are the boot-run's, printed by words that exist only because the seed
\ copied the blob, registered the records and patched the relocation sites of THIS
\ engine - so those are what say the capture booted.
: BATCH-RUN ( ptr u8 n -- ) {: p:ptr pu:n :}
   PROC-ARGV-RESET
   HB$ >LEN  p pu >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: BATCH-OK ( -- )
   s" 7 6 * . cr" BATCH-RUN ;

: DIGIT? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and ;

: DIGITS-END ( n -- n )            \ from an index in OUT, the first non-digit at or after it
   begin dup OUT-U @ < while
      dup OUT + c@ DIGIT? 0= if exit then
      1+
   repeat ;

\ The digits a boot-run report printed after its label, as a string. Taking the
\ SPAN rather than searching for an expected substring is what lets the named-code
\ literal case compare two reports against each other: its value is an address in
\ the engine that printed it, so no fixed text can stand for it. An absent label
\ answers an empty span, which no expectation matches and which the code-literal
\ case rejects outright before it compares.
: REPORT$ ( ptr u8 n -- ptr u8 n ) {: m:ptr mu:n :}
   OUT$ m mu FIND-SUB MATCH option
     none OF OUT-U @ ENDOF                      \ absent: start at the end -> empty span
     some OF IDX>N mu + ENDOF
   ;MATCH {: st:n :}
   OUT st +  st DIGITS-END st - ;

\ A report's value, asserted against a number the FIXTURE fixed. The two live in
\ different files on purpose - the magic is written in test/aot-wid-build.f and
\ read here - so a fixture that quietly stopped carrying its value cannot also
\ quietly move the expectation.
: REPORT= ( ptr u8 n ptr u8 n -- ) {: m:ptr mu:n w:ptr wu:n :}
   m mu REPORT$ w wu STR= TTRUE ;

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
   OUT$ s" 42" CONTAINS? TTRUE
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
   OUT$ s" 42" CONTAINS? TTRUE
   \ The boot-run resolves its entry words through LFIND, and for an EXT record
   \ LFIND compares the token against the bytes [24] points at - which the seed
   \ sets to the baked name pool. A wrong pointer finds nothing and the boot-run
   \ exits $52 with no report at all, so the printed magic is the pointer's proof.
   s" the out-of-line-named word is found by that name at boot" T-LABEL
   s" awb-ext=" s" 6510767442340633178" REPORT= ;

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
   OUT$ s" 42" CONTAINS? TTRUE
   \ Three answers were possible and only one is the row working: 11 is the
   \ quotation the chain originally pointed at (the rebase ran, the named row did
   \ not), 22 is the word the row names, and the zero the capture left is a jump
   \ to address 0. 22 is the one that says the seed resolved the NAME.
   s" the named code site resolves to the word it names, not the chain's own" T-LABEL
   s" awb-xt=" s" 22" REPORT= ;

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
   OUT$ s" 42" CONTAINS? TTRUE
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
   S\" : XLP ( -- ) s\" xl-live=\" type ['] HH0 . cr ; XLP" BATCH-RUN
   s" the code-literal variant still runs a batch program" T-LABEL
   RC @ 0 T=
   s" the baked literal is this engine's own entry for the word it names" T-LABEL
   s" awb-xl=" REPORT$ {: a:ptr u:n :}
   u 0 T<>
   a u  s" xl-live=" REPORT$  STR= TTRUE ;

: BODY ( -- )
   PROBE-BIG-WINDOW
   PROBE-EXT-NAME
   PROBE-XTSITE
   PROBE-XTLIT
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

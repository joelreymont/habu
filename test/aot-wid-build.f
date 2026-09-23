\ aot-wid-build.f - build a protected-WID partial native image.
\
\ Run as `bin/hb --load test/aot-wid-build.f` with HB_TMP pointing at a private
\ directory; on success it writes an `hb-pwid` engine into that directory. The
\ default variant carries the REPL and two packages of this fixture's own,
\ one protected. Its cold boot restores one protected wordlist the ordinary
\ source prefix has not got.
\ Nothing in production is touched: the protection is asked for inside this
\ throwaway variant's window, through `prot-wid-add` - the same public word any
\ package uses to seal its own word-list - and the capture derives what travels
\ from the live band (aot-capture.f ACAP-PWIN-CAPTURE) exactly as the real
\ metabuild does.
\
\ WHAT A CAPTURE CARRIES IS WINDOW-RELATIVE, which is why this file cannot name
\ the id the built engine ends up protecting. A captured protected WID is stored
\ as its offset from the window's first wordlist id (AOT-BUF:AOT-WID-W0) and the
\ seed rebases each one onto the booting engine's WIDN (habu2.f
\ AOT-WINDOW:SEAL-WIDS,), so no build-host id survives the cut. The fixture
\ therefore protects a package it CAN name, and test/aot-wid-suite.f asks the
\ built engine which id that package got (tools/pkg-wid-probe.f).
\
\ The current native writer first emits an empty cold host. That host loads the
\ real prefix and compiles the REPL and fixture in one partial window, using
\ the JIT instruction shapes the offset and inlining cases below describe.
\ All fixture definitions precede the one effect-payload freeze; repeated
\ captures keep that same membership and measure the live protection band.
\ The artifact crosses to a fresh optimizing native writer process. Its cold
\ product reads the restoring checkout's prefix before seeding, so a private
\ top-row copy can establish a real target WID collision.
\
\ The modes below are selected by environment so one builder serves every case
\ its companion suites need (HABU_AOT_GATE serves two) -
\ test/aot-wid-suite.f, test/aot-wide-format-suite.f and the PTY half in
\ test/aot-data-span-forge.f:
\
\   (default)          define two packages inside the capture window and protect
\                      one of them, then check the capture's contract against the
\                      live band on the host: the band carries PROT-REG-TAG, a
\                      capture taken before the protection records no row at all,
\                      the rows taken after it are exactly the live protected WIDs
\                      inside the window, and the host's own protected WIDs - all
\                      of which sit below the window - do not travel.
\   HABU_PWID_BAD=N    hand N to `prot-wid-add` inside the window and re-capture.
\                      N at or above PROT-WID-MAX has no bit in the band and the
\                      primitive that owns the bound refuses it; N=0 sets the one
\                      bit no registry may carry and the capture refuses the band.
\                      Either way the build dies named and no engine appears.
\   HABU_AOT_SPAN=N    overwrite the captured AOT DATA span (the sibling
\                      test/aot-data-span-forge.f forge; see SPAN-FORGE-LINE).
\   HABU_AOT_WID_SKEW=N  move the captured wid window's base up by N after the
\                      capture, so the baked records name wordlists BELOW the
\                      baked window. The seed must refuse at boot.
\   HABU_AOT_WID_SPAN=N  the same forge on the other side: set the baked window's
\                      span to N, so the records name wordlists past its end.
\   HABU_AOT_WID_NARROW=N  declare the window's wordlist span N ids late, so a
\                      wordlist the window really made is outside it. The CAPTURE
\                      must refuse, naming the record.
\   HABU_AOT_BAKE=1    put an INITIALISED data cell and a word that reads it inside
\                      the capture window, and run that word from the boot-run
\                      list. The built engine reports the value when it is entered
\                      on a tty, and the value is zero unless the window's DATA
\                      content travelled into the image.
\   HABU_AOT_TRAP=1    put a `defer` inside the capture window and run a word that
\                      CALLS it from the boot-run list, installing nothing. Its
\                      dispatch cell is a declared address cell, so the capture
\                      zeroed it and the seed re-trapped it; entering the engine
\                      must die "defer: unset execution vector" rather than branch
\                      into whatever the baked bytes held.
\   HABU_AOT_BIG=1     grow the capture window past the 64 KiB world the format
\                      used to live in (dot habu-widen-the-aot-089f5faf): compile
\                      BIG-FILLER-N filler words, then a data cell, a callee and a
\                      reporter ABOVE them, and capture the complete window. The build then asserts the three offsets that
\                      no u16 field could have held - the blob length, the highest
\                      call-site blob offset and the highest DATA-site blob offset
\                      are each past 65535 - and dies named if any of them is not,
\                      so the fixture cannot quietly stop testing what it is for.
\                      It prints the three measurements for its callers to read.
\   HABU_AOT_EXT=1     put a word whose NAME is longer than a dictionary record can
\                      hold inside the capture window, and run it from the
\                      boot-run list. Such a record keeps its name out of line,
\                      which the capture used to refuse outright; the build now
\                      asserts one was captured, and the engine can only report
\                      when the boot-run's LFIND matches that out-of-line name.
\   HABU_AOT_XTLIT=1   put a word holding `['] HH0` - a CODE literal naming a
\                      PRE-WINDOW word - inside the capture window. The compile
\                      handler emits that chain into the window word's own body, so
\                      the inliner decline cannot reach it and the capture used to
\                      die named. It is now carried as a name-keyed code row: the
\                      build asserts, over the fixture word's own captured record,
\                      exactly one such row inside its body naming HH0 and no
\                      rebased code site there, and the engine reports the ticked
\                      value at boot for its caller to compare against its own.
\   HABU_AOT_PREWIN=1  put a word that names a PREFIX data word inside the capture
\                      window, and run it from the boot-run list. The prefix word's
\                      body is short enough to inline, so the copy used to carry an
\                      address below the window's DATA span and the capture died
\                      named; the engine now declines that copy and emits the call,
\                      which the seed relocates by name. The build asserts, over
\                      the fixture word's own captured record, that the body holds
\                      no DATA relocation site and does hold a call to the prefix
\                      word.
\   HABU_AOT_GATE=1|2  compile a call to a QUALIFIED PREFIX word inside the capture
\                      window, so the seed resolves that name through the package's
\                      public word-list and the AOT boot gate judges the wid the
\                      lookup used. Mode 1's callee is in a SEALED prefix package
\                      (CODE-RECLAIM), mode 2's in an unsealed one (CHECKER-TAPE),
\                      and both engines must boot: calling a public word of a
\                      sealed package is what checked source does. The pair exists
\                      for the mutation - deleting the gate's public-slot admit
\                      kills mode 1 and leaves mode 2 alone - and the build asserts
\                      each package's seal status before it uses it.
\   HABU_AOT_D0_SKEW=N run the capture a SECOND time over the same window with
\                      its DATA span start raised by N bytes. Hand it an N past
\                      the whole span and the window then contains none of the
\                      address chains its own blob holds, which is the case the
\                      capture must refuse rather than bake or skip - so the
\                      build dies named and no engine appears.
\
\ Its companion test/aot-wid-suite.f spawns this builder in a child process and
\ then probes the resulting hb-pwid to prove the protected-WID bitmap is restored
\ at engine startup, before any batch program runs.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require test/cold-engine.f

package AOT-WID-BUILD

\ The large mode contributes one checked definition per filler row.
$20000 constant DRV-CAP
$10000 constant IO-CAP
120000 constant CHILD-TIMEOUT-MS
74 constant BUILD-RC

create DRV-BUF DRV-CAP allot   variable DRV-U
create DRV-PATH-BUF FS-PATH-CAP allot   variable DRV-PATH-U
create ROOT-BUF FS-PATH-CAP allot variable ROOT-U
create COLD-BUF FS-PATH-CAP allot variable COLD-U
create ART-BUF FS-PATH-CAP allot variable ART-U
create IMAGE-BUF FS-PATH-CAP allot variable IMAGE-U
create PATCH-BUF FS-PATH-CAP allot variable PATCH-U
create OUT IO-CAP allot
create ERR IO-CAP allot
variable WINDOW-ENDED

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: COLD$ ( -- ptr u8 n ) COLD-BUF COLD-U @ ;
: ART$ ( -- ptr u8 n ) ART-BUF ART-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: PATCH$ ( -- ptr u8 n ) PATCH-BUF PATCH-U @ ;

: PATHS ( -- )
   s" HB_TMP" GETENV {: a:ptr u:n :}
   u 0= u FS-PATH-CAP > or if
      s" aot-wid-build: HB_TMP must name a private directory" BUILD-RC die then
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ s" hb-cold" COLD-BUF JOIN-PATH COLD-U !
   ROOT$ s" window.aot" ART-BUF JOIN-PATH ART-U !
   ROOT$ s" hb-pwid" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ s" pwid-patch.f" PATCH-BUF JOIN-PATH PATCH-U ! ;

: DRV-PATH$ ( -- ptr u8 n )
   DRV-PATH-BUF DRV-PATH-U @ ;

: DRV-PATH! ( -- )                 \ <HB_TMP>/pwid-driver.f
   ROOT$ s" pwid-driver.f" DRV-PATH-BUF JOIN-PATH DRV-PATH-U ! ;

: DRV-RESET ( -- )
   0 DRV-U ! ;

: DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DRV-U @ u + DRV-CAP > if s" aot-wid-build: driver buffer overflow" BUILD-RC die then
   a DRV-BUF DRV-U @ + u BYTE-COPY
   DRV-U @ u + DRV-U ! ;

: DRV-NL ( -- )
   10 DRV-BUF DRV-U @ + c!
   DRV-U @ 1+ DRV-U ! ;

: DRV-LINE ( ptr u8 n -- ) DRV+ DRV-NL ;

\ Every reopened AOT-CAPTURE block below reads the capture buffers by their bare
\ names, and those are package AOT-BUF's public surface (src/habu/aot-decl.f).
\ aot-capture.f's own `using AOT-BUF` closes with that file's `;package`, and this
\ text is a fresh eval frame besides, so each generated block has to open its own
\ import. One word emits the pair and DRV-IMPORT-CHECK proves no block skipped it.
: DRV-AOT-CAPTURE ( -- )
   s" package AOT-CAPTURE" DRV-LINE
   s" using AOT-BUF" DRV-LINE ;

: DRV-AT? ( n ptr u8 n -- bool ) {: off:n a:ptr u:n :}
   off u + DRV-U @ > if 0 0= 0= exit then
   DRV-BUF off + u  a u STR= ;

: DRV-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}          \ occurrences in the assembled driver
   0  DRV-U @ 0 ?do  i a u DRV-AT? if 1+ then  loop ;

\ Structural, not textual: it counts openers and counts opener-followed-by-import,
\ so a block that opens the package without the import makes the two disagree.
: DRV-IMPORT-CHECK ( -- )
   S\" package AOT-CAPTURE\n" DRV-COUNT
   S\" package AOT-CAPTURE\nusing AOT-BUF\n" DRV-COUNT
   <> if s" aot-wid-build: a generated AOT-CAPTURE block has no `using AOT-BUF`" BUILD-RC die then ;

create DRV-CH 1 allot
: DRV-CH+ ( n -- ) {: c:n :}
   c DRV-CH c!  DRV-CH 1 DRV+ ;
: DRV-U+ ( n -- ) {: v:n :}        \ decimal, for the generated filler names
   v 10 >= if v 10 / recurse then
   v 10 mod 48 + DRV-CH+ ;

\ Forge the emitted scalar after the ordinary artifact reader and image writer
\ accept the valid capture. The generated patch checks its bound label, full
\ cell span and original value before rebuilding/signing that image.
: SPAN-FORGE-LINE ( -- )
   s" HABU_AOT_SPAN" GETENV {: v:ptr vu:n :}
   vu 0 > if
      v vu DRV+
      s"  AOT-BUF:AOT-DATA-SIZE @ LAOTDATASIZE LABEL@ PATCH-CELL" DRV-LINE
   then ;

\ Optional wid-window forges (dot habu-rebase-captured-wids-54dec421). The seed
\ rebases every captured wid through the baked window, and refuses one the window
\ does not contain - on either side. The capture refuses such a record at capture
\ time, so corrupt the emitted scalar AFTER the normal writer. The baked base is
\ the constant every captured offset counts from (src/habu/aot-decl.f
\ AOT-BUF:WID-REL-BASE), so raising it puts the first N offsets below the
\ window; shrinking the span puts them past its end. Both need a fixture that captures a
\ non-zero wid at all - the REPL sources define no package, so HABU_AOT_GATE
\ supplies the package these are combined with.
: WID-FORGE-LINE ( -- )
   s" HABU_AOT_WID_SKEW" GETENV {: v:ptr vu:n :}
   vu 0 > if
      s" AOT-BUF:WID-REL-BASE " DRV+  v vu DRV+
      s"  + AOT-BUF:WID-REL-BASE AOT-WINDOW:LWIDW0 LABEL@ PATCH-CELL" DRV-LINE
   then
   s" HABU_AOT_WID_SPAN" GETENV {: p:ptr pu:n :}
   pu 0 > if
      p pu DRV+
      s"  AOT-BUF:AOT-WID-SPAN @ AOT-WINDOW:LWIDSPAN LABEL@ PATCH-CELL" DRV-LINE
   then ;

\ --- the capture window and its fixture source --------------------------------
: BAD-ENV$ ( -- ptr u8 n )       s" HABU_PWID_BAD" GETENV ;
: SKEW-ENV$ ( -- ptr u8 n )      s" HABU_AOT_D0_SKEW" GETENV ;
: SPAN-ENV$ ( -- ptr u8 n )      s" HABU_AOT_SPAN" GETENV ;
: BAKE-ENV$ ( -- ptr u8 n )      s" HABU_AOT_BAKE" GETENV ;
: TRAP-ENV$ ( -- ptr u8 n )      s" HABU_AOT_TRAP" GETENV ;
: BIG-ENV$ ( -- ptr u8 n )       s" HABU_AOT_BIG" GETENV ;
: EXT-ENV$ ( -- ptr u8 n )       s" HABU_AOT_EXT" GETENV ;
: XL-ENV$ ( -- ptr u8 n )        s" HABU_AOT_XTLIT" GETENV ;
: PREWIN-ENV$ ( -- ptr u8 n )    s" HABU_AOT_PREWIN" GETENV ;
: GATE-ENV$ ( -- ptr u8 n )      s" HABU_AOT_GATE" GETENV ;

\ The plain protected-WID fixture is the fallback mode, but two knobs reach that
\ point with a window of their own to keep: the D0-skew refusal re-captures the
\ closed REPL window, and the DATA-span forge
\ (test/aot-data-span-forge.f) bakes that same window with a forged span. Neither
\ wants two packages of ours inside it.
: PLAIN-MODE? ( -- bool )
   SKEW-ENV$ nip 0 =  SPAN-ENV$ nip 0 =  and ;

\ Re-run the real capture over the real window with the DATA span start moved.
\ AOT-ARM's window cells still hold the span just latched, so raising
\ D0 and handing the same WINDOW$ to the production entry point (AOT-CAPTURE's
\ public CAPTURE) gives a window that cannot describe its own contents - a poked
\ wrong value, not a stand-in. The line is emitted inside the reopened
\ AOT-CAPTURE package, where CAPTURE resolves bare.
: SKEW-BODY ( ptr u8 n -- ) {: v:ptr vu:n :}
   s" AOT-ARM:D0 @ " DRV+  v vu DRV+  s"  + AOT-ARM:D0 !" DRV-LINE
   s" AOT-ARM:WINDOW$ CAPTURE" DRV-LINE ;

\ The D0-skew refusal is the only body that has to sit inside a reopened
\ AOT-CAPTURE block: it re-enters the capture by its bare name. Every other mode
\ works at top level, so no other build carries the block.
: SKEW-MODE-LINES ( -- )
   SKEW-ENV$ {: k:ptr ku:n :}
   ku 0= if exit then
   s" AOT-ARM:WINDOW-CLOSE" DRV-LINE
   -1 WINDOW-ENDED !
   s" AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE" DRV-LINE
   DRV-AOT-CAPTURE
   k ku SKEW-BODY
   s" ;package" DRV-LINE ;

\ Load the real REPL source inside the fixture window. The terminal and REPL
\ files are INCLUDEd because the window has to compile them here; the debugger is
\ one REQUIRE because src/habu/debug.f requires the watch cells and the stepper
\ itself, and naming all three again loaded each of them twice (`duplicate
\ definition: BPW-MAX`). The cold host provides none of the four, so the require
\ reads the file.
: CAPTURE-REPL-LINES ( -- )
   s" AOT-ARM:WINDOW-OPEN" DRV-LINE
   HB-TARGET-LINUX? if s" include src/os/linux/repl-term.f"
   else s" include src/os/macos/repl-term.f" then DRV-LINE
   s" include src/habu/repl.f" DRV-LINE
   s" require src/habu/debug.f" DRV-LINE ;

\ --- the window-content fixtures ----------------------------------------------
\ Define at top level, then close the real capture window after both the REPL
\ and the fixture definitions. AOT-ARM owns every span and the checker payload.
\ The boot-run list is re-stated because CAPTURE resets it, and the three REPL
\ entries come first so the engine still installs its REPL before the fixture runs.
\ HABU_AOT_WID_NARROW=N pokes the window's wordlist END back down by N after it
\ was latched, so the window declares fewer ids than it really made - the
\ fixture's package is the last thing the window creates, so the short end is the
\ end that reaches it. The capture must refuse at build time, naming the record;
\ the boot's own refusal never gets the chance.
: WID-NARROW-LINE ( -- )
   s" HABU_AOT_WID_NARROW" GETENV {: n:ptr nu:n :}
   nu 0= if exit then
   s" AOT-ARM:W1 @ " DRV+  n nu DRV+  s"  - AOT-ARM:W1 !" DRV-LINE ;

\ Close only once, after the fixture definitions. Subsequent captures retain
\ the same frozen membership even if tooling later allocates or defines helpers.
: RECAPTURE-LINE ( -- )
   WINDOW-ENDED @ 0= if
      s" AOT-ARM:WINDOW-CLOSE" DRV-LINE
      WID-NARROW-LINE
      -1 WINDOW-ENDED !
   then
   s" AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE" DRV-LINE ;

: REPL-BOOTRUN-LINES ( -- )
   S\" s\" INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE
   S\" s\" BPW-INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE
   S\" s\" S-INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ --- the protected-WID fixture (the default build) -----------------------------
\ TWO PACKAGES, ONE PROTECTED. The unprotected one is defined first so the
\ protected one's window-relative offset is not zero: a seed that dropped the
\ offset and protected WIDN itself would still satisfy a fixture whose only
\ protected wordlist was the window's first. Each publishes one word, so each has
\ a package record the capture carries and the built engine can be asked about by
\ name - which is how test/aot-wid-suite.f learns the two rebased ids.
\
\ THE CHECK TAKES TWO CAPTURES OVER ONE WINDOW, because the capture's contract has
\ two halves and one window can show both. The first capture, with nothing in the
\ window protected, is the EMPTY registry: it must be accepted and must record no
\ row. The second, after `prot-wid-add`, must record exactly the live protected
\ WIDs inside the window - no more (the host's own 100-odd protected word-lists
\ sit below the window and are deliberately dropped, since replaying them would
\ resurrect the discarded namespace) and no fewer (the fixture's own must be
\ there, at a non-zero offset). Both run in the METABUILD HOST through the
\ capture's own words, which is the only place that contract can be checked
\ against a live band.
: PROT-CHECK-DEF ( -- )
   DRV-AOT-CAPTURE
   s" : PROT-ROW ( n -- n ) 4 * AOT-PWIN-BUF@ + ACAP-W32@ ;" DRV-LINE
   s" : PROT-ROW? ( n -- bool ) {: rel:n :}" DRV-LINE
   s"    AOT-PWIN-N @ 0 ?do i PROT-ROW rel = if 0 0= unloop exit then loop  0 0= 0= ;" DRV-LINE
   s" : PROT-LIVE-IN ( -- n )" DRV-LINE
   s"    0  AOT-WID-SPAN @ 0 ?do i AOT-WID-W0 @ + ACAP-LIVE-PWID? if 1+ then loop ;" DRV-LINE
   s" : PROT-LIVE-BELOW ( -- n )" DRV-LINE
   s"    0  AOT-WID-W0 @ 0 ?do i ACAP-LIVE-PWID? if 1+ then loop ;" DRV-LINE
   s" : PROT-ROWS-LIVE? ( -- bool )" DRV-LINE
   s"    AOT-PWIN-N @ 0 ?do" DRV-LINE
   s"       i PROT-ROW {: rel:n :}" DRV-LINE
   s"       rel AOT-WID-SPAN @ < 0= if 0 0= 0= unloop exit then" DRV-LINE
   s"       rel AOT-WID-W0 @ + ACAP-LIVE-PWID? 0= if 0 0= 0= unloop exit then" DRV-LINE
   s"    loop  0 0= ;" DRV-LINE
   s" : PROT-CHECK ( -- )" DRV-LINE
   s"    AOT-LIVE-DATA PROT-REG-TAG-CELL + AOT-CELL@ PROT-REG-TAG <> if" DRV-LINE
   S\"       s\" aot-wid-build: metabuild host band carries no bitmap tag\" 74 die then" DRV-LINE
   s"    AWB-PWIN0 @ 0 <> if" DRV-LINE
   S\"       s\" aot-wid-build: the window held a protected WID before the fixture asked\" 74 die then" DRV-LINE
   s"    PROT-ROWS-LIVE? 0= if" DRV-LINE
   S\"       s\" aot-wid-build: a captured row is not a live protected WID in the window\" 74 die then" DRV-LINE
   s"    PROT-LIVE-IN AOT-PWIN-N @ <> if" DRV-LINE
   S\"       s\" aot-wid-build: the capture and the live band disagree in the window\" 74 die then" DRV-LINE
   s"    PROT-LIVE-BELOW 0= if" DRV-LINE
   S\"       s\" aot-wid-build: the host protects nothing below the window\" 74 die then" DRV-LINE
   s"    AWB-PROT-WID @ AOT-WID-W0 @ - {: prel:n :}" DRV-LINE
   s"    prel 0 > 0= if" DRV-LINE
   S\"       s\" aot-wid-build: the protected wordlist is the window's first\" 74 die then" DRV-LINE
   s"    prel PROT-ROW? 0= if" DRV-LINE
   S\"       s\" aot-wid-build: the protected wordlist did not travel\" 74 die then" DRV-LINE
   s"    AWB-OPEN-WID @ AOT-WID-W0 @ - PROT-ROW? if" DRV-LINE
   S\"       s\" aot-wid-build: an unprotected window wordlist travelled\" 74 die then" DRV-LINE
   S\"    s\" aot-wid-build: pwin-rows \" type AOT-PWIN-N @ . cr" DRV-LINE
   S\"    s\" aot-wid-build: pwin-rel \" type prel . cr" DRV-LINE
   S\"    s\" aot-wid-build: prot-below \" type PROT-LIVE-BELOW . cr ;" DRV-LINE
   s" PROT-CHECK" DRV-LINE
   s" ;package" DRV-LINE ;

: PROT-FIXTURE-LINES ( -- )
   s" variable AWB-OPEN-WID" DRV-LINE
   s" variable AWB-PROT-WID" DRV-LINE
   s" variable AWB-PWIN0" DRV-LINE
   s" package AWBOPEN" DRV-LINE
   s" public" DRV-LINE
   s" : AWB-OPEN-MARK ( -- n ) 1 ;" DRV-LINE
   s" get-current AWB-OPEN-WID !" DRV-LINE
   s" ;package" DRV-LINE
   s" package AWBPROT" DRV-LINE
   s" public" DRV-LINE
   s" : AWB-PROT-MARK ( -- n ) 2 ;" DRV-LINE
   s" get-current AWB-PROT-WID !" DRV-LINE
   s" ;package" DRV-LINE
   RECAPTURE-LINE                       \ the empty pass: nothing is protected yet
   s" AOT-BUF:AOT-PWIN-N @ AWB-PWIN0 !" DRV-LINE
   s" AWB-PROT-WID @ prot-wid-add" DRV-LINE
   RECAPTURE-LINE
   PROT-CHECK-DEF
   REPL-BOOTRUN-LINES ;

\ The two refusals the live capture path owns, both reached through the word that
\ owns the bound. An id at or above PROT-WID-MAX has no bit in the band, so
\ `prot-wid-add` refuses it (exit 84) before any capture is asked anything - the
\ memory-safety argument for a prim taking a caller-supplied index, whose
\ in-process twin is in test/seal.f. WID 0 is not a wordlist, so a band with bit 0
\ set is not a registry, and it is the RE-CAPTURE below that asks the capture
\ about it (exit 74). Neither build reaches an image.
: BAD-FIXTURE-LINES ( ptr u8 n -- ) {: v:ptr vu:n :}
   v vu DRV+  s"  prot-wid-add" DRV-LINE
   RECAPTURE-LINE ;

\ An initialised cell plus a word that reads it. The word's reference to the cell
\ is a DATA address literal inside the window, so the value only reaches the
\ report if the content travelled AND that literal was rebased onto the seeded DP.
: BAKE-FIXTURE-LINES ( -- )
   s" create AWB-CELL 8 allot" DRV-LINE
   s" $5A5AC0DEC0DE5A5A AWB-CELL !" DRV-LINE
   S\" : AWB-REPORT ( -- ) s\" awb-cell=\" type AWB-CELL @ . cr ;" DRV-LINE
   RECAPTURE-LINE
   REPL-BOOTRUN-LINES
   S\" s\" AWB-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ A deferred word nothing installs, called from the boot-run. Its dispatch cell is
\ a declared address cell inside the window, so the capture zeroed it and the seed
\ wrote the trap xt back; calling it must reach DEFER-UNSET's named die.
: TRAP-FIXTURE-LINES ( -- )
   s" defer AWB-VEC ( -- )" DRV-LINE
   s" : AWB-CALL ( -- ) AWB-VEC ;" DRV-LINE
   RECAPTURE-LINE
   REPL-BOOTRUN-LINES
   S\" s\" AWB-CALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ --- the big-window fixture (dot habu-widen-the-aot-089f5faf) ------------------
\ THE POINT IS THE OFFSETS, not the size. Until the format widened, a call-site
\ row, a DATA-site offset and a CODE-site offset were each u16, so a captured
\ window could not describe anything past its 65535th byte and the capture died
\ at AOT-BLOB-CAP before it ever got there. This fixture builds a window that is
\ beyond that, with the three things that have to survive it defined ABOVE
\ the filler: a data cell, a callee, and a reporter that calls the callee and
\ prints the cell. So the
\ engine can only report the magic if a call site AND a DATA site whose blob
\ offsets do not fit sixteen bits were both recorded and patched at boot.
\ The filler words carry no calls of their own; they exist to push the three
\ words that matter past the old ceiling.
1024 constant BIG-FILLER-N         \ 80 bytes each on the current JIT; keeps the sites past 64 KiB

: BIG-FILLER ( -- )
   BIG-FILLER-N 0 ?do
      s" : AWB-BIG-F" DRV+  i DRV-U+
      s"  ( n -- n ) dup + dup + dup + dup + dup + dup + dup + dup + ;" DRV-LINE
   loop ;

\ The three measurements, taken from the capture's own tables through its own
\ private row readers, and each one fatal if it does not clear 65535. A window
\ that stopped exceeding the old ceiling would make this fixture prove nothing,
\ so it ends the build instead of quietly passing.
: BIG-CHECK-DEF ( -- )
   DRV-AOT-CAPTURE
   s" : BIG-MAX-SITE ( -- n )" DRV-LINE
   s"    0 AOT-SITE-N @ 0 ?do i ACAP-SITE-ROW ACAP-W32@ max loop ;" DRV-LINE
   s" : BIG-MAX-DSITE ( -- n )" DRV-LINE
   s"    0 AOT-DSITE-N @ 0 ?do AOT-DSITE-BUF@ i 4 * + ACAP-W32@ max loop ;" DRV-LINE
   s" : BIG-CHECK ( -- )" DRV-LINE
   s"    AOT-BLOB-LEN @ $10000 > 0= if" DRV-LINE
   S\"       s\" aot-wid-build: big-window blob still fits the old 64KB world\" 74 die then" DRV-LINE
   s"    BIG-MAX-SITE $FFFF > 0= if" DRV-LINE
   S\"       s\" aot-wid-build: big-window call site still fits a u16 offset\" 74 die then" DRV-LINE
   s"    BIG-MAX-DSITE $FFFF > 0= if" DRV-LINE
   S\"       s\" aot-wid-build: big-window DATA site still fits a u16 offset\" 74 die then" DRV-LINE
   S\"    s\" aot-wid-build: big-blob \" type AOT-BLOB-LEN @ . cr" DRV-LINE
   S\"    s\" aot-wid-build: big-site \" type BIG-MAX-SITE . cr" DRV-LINE
   S\"    s\" aot-wid-build: big-dsite \" type BIG-MAX-DSITE . cr ;" DRV-LINE
   s" BIG-CHECK" DRV-LINE                  \ private to the package: run it while the block is open
   s" ;package" DRV-LINE ;

: BIG-FIXTURE-LINES ( -- )
   BIG-FILLER
   s" create AWB-BIG-CELL 8 allot" DRV-LINE
   s" $5A5AB16B16B15A5A AWB-BIG-CELL !" DRV-LINE
   s" : AWB-BIG-CORE ( -- ) 0 dup + dup + dup + dup + dup + dup + dup + dup + drop ;" DRV-LINE
   s" : AWB-BIG-LAST ( -- n ) AWB-BIG-CORE AWB-BIG-CELL @ ;" DRV-LINE
   S\" : AWB-BIG-REPORT ( -- ) s\" awb-big=\" type AWB-BIG-LAST . cr ;" DRV-LINE
   RECAPTURE-LINE
   BIG-CHECK-DEF
   REPL-BOOTRUN-LINES
   S\" s\" AWB-BIG-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ --- the out-of-line name fixture (dot habu-widen-the-aot-089f5faf) ------------
\ A name longer than DNAME-INL is not kept in its dictionary record: the definer
\ writes the bytes at CP and the record's [24] cell points at them. The capture
\ used to refuse such a record outright ("rec has EXT name (uncompactable)"), and
\ the compiler chain has 45 of them. Now the name travels in the deduped pool
\ like every other name and the seed points [24] at the pooled bytes.
\ WHAT MAKES THE BOOT HALF DIRECT: the reporter's own name is the long one, and
\ the boot-run list resolves its entry words through LFIND - which, for an EXT
\ record, compares the caller's token against the bytes at [24]. A wrong pointer
\ there cannot find the word, and EM-AOT-BOOTRUN exits $52 instead of reporting.
: EXT-CHECK-DEF ( -- )
   DRV-AOT-CAPTURE
   s" : EXT-REC-N ( -- n )" DRV-LINE
   s"    0 AOT-REC-N @ 0 ?do i ACAP-CREC-DST 12 + c@ 2 and 0= 0= if 1+ then loop ;" DRV-LINE
   s" : EXT-CHECK ( -- )" DRV-LINE
   s"    EXT-REC-N {: n:n :}" DRV-LINE
   s"    n 0= if" DRV-LINE
   S\"       s\" aot-wid-build: ext fixture captured no out-of-line name\" 74 die then" DRV-LINE
   S\"    s\" aot-wid-build: ext-recs \" type n . cr ;" DRV-LINE
   s" EXT-CHECK" DRV-LINE
   s" ;package" DRV-LINE ;

: EXT-FIXTURE-LINES ( -- )
   s" create AWB-EXT-CELL 8 allot" DRV-LINE
   s" $5A5AE47E47E45A5A AWB-EXT-CELL !" DRV-LINE
   S\" : AWB-A-DELIBERATELY-LONG-REPORT-NAME ( -- ) s\" awb-ext=\" type AWB-EXT-CELL @ . cr ;" DRV-LINE
   RECAPTURE-LINE
   EXT-CHECK-DEF
   REPL-BOOTRUN-LINES
   S\" s\" AWB-A-DELIBERATELY-LONG-REPORT-NAME\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ --- what both body checks below need ------------------------------------------
\ Each of them finds ONE captured dict record by name and then reads the capture's
\ tables over that record's blob span, so both need the same byte compare and the
\ same record search. One emitted definition of each, in one reopened AOT-CAPTURE
\ block that the caller's own lines continue: the modes are mutually exclusive
\ (FIXTURE-LINES picks one), so a single copy is all any driver ever carries.
: BODY-CHECK-OPEN ( -- )
   DRV-AOT-CAPTURE
   s" : AWB-NAME= ( ptr u8 ptr u8 n -- bool ) {: a:ptr b:ptr u:n :}" DRV-LINE
   s"    u 0 ?do a i + c@ b i + c@ <> if 0 0= 0= unloop exit then loop  0 0= ;" DRV-LINE
   s" : AWB-REC-BY-NAME ( ptr u8 n -- n ) {: a:ptr u:n :}" DRV-LINE
   s"    AOT-REC-N @ 0 ?do" DRV-LINE
   s"       i ACAP-REC-DST {: v:ptr :}" DRV-LINE
   s"       v 16 + ACAP-W32@ u = if" DRV-LINE
   s"          v 24 + a u AWB-NAME= if i unloop exit then" DRV-LINE
   s"       then" DRV-LINE
   s"    loop  -1 ;" DRV-LINE ;

\ --- the pre-window CODE literal fixture (dot habu-widen-the-aot-089f5faf) ------
\ THE CASE THE INLINER DECLINE CANNOT REACH. `['] HH0` on a PREFIX word compiles a
\ code-address chain straight into the window word's own body (habu2.f C-BTICK ->
\ C-CODE-ADDR), so there is no copy to decline: the chain is the window's own. Its
\ value is a prefix code address, which the window's DATA span does not hold and
\ its code span does not hold either, and on the base this exact fixture ends the
\ build - "aot-capture: recorded address site at blob offset N carries V which is
\ in neither the window's DATA span nor its code span", exit 74, no image. The
\ capture now recognises it as a call target that is not a BL and writes a
\ name-keyed code row; the seed resolves HH0 in the engine it is booting.
\
\ WHAT THE CHECK ASSERTS, and why each half is needed. It finds AWB-XL-REPORT's own
\ captured dict record BY NAME and takes its blob span from that record, then
\ requires THREE things over that span: exactly one named code row inside it (more
\ than one means the fixture grew a second literal and the case stopped being
\ about the one under test), that row's pooled name is HH0 (a row naming something
\ else would resolve to the wrong entry and still "pass" a count), and NO rebased
\ code site inside it (which is the other way the classification could have gone -
\ the two lists are exclusive, so this is what says the site took the named branch
\ rather than being carried b0-relative).
\
\ WHAT THE BOOT HALF ASSERTS. The reporter prints the ticked value from INSIDE the
\ window. Its caller boots the same engine again with a program that ticks the same
\ word from OUTSIDE, in ordinary compiled code, and requires the two numbers to be
\ equal. That needs no fixed constant, so it survives ASLR, and neither of the two
\ wrong answers can produce it: the host's address (had the chain been carried
\ verbatim) is not this engine's, and the zero the capture leaves is not either.
: XL-CHECK-DEF ( -- )
   BODY-CHECK-OPEN
   s" variable XL-HIT  variable XL-CS" DRV-LINE
   S\" : XL-HH0$ ( -- ptr u8 n ) s\" HH0\" ;" DRV-LINE
   s" : XL-ROW ( n -- ptr u8 ) 8 * AOT-XTSITE:BUF@ + ;" DRV-LINE
   s" : XL-POOL= ( n ptr u8 n -- bool ) {: noff:n a:ptr u:n :}" DRV-LINE
   s"    AOT-NAMES-BUF@ noff + c@ u = 0= if 0 0= 0= exit then" DRV-LINE
   s"    AOT-NAMES-BUF@ noff 1+ + a u AWB-NAME= ;" DRV-LINE
   s" : XL-ROW-IN ( n n -- n ) {: start:n clen:n :}" DRV-LINE
   s"    -1 XL-HIT !" DRV-LINE
   s"    AOT-XTSITE:N @ 0 ?do" DRV-LINE
   s"       i XL-ROW ACAP-W32@ {: off:n :}" DRV-LINE
   s"       off start >= off start clen + < and if" DRV-LINE
   s"          XL-HIT @ 0 >= if" DRV-LINE
   S\"             s\" aot-wid-build: xtlit body holds more than one named row\" 74 die then" DRV-LINE
   s"          i XL-HIT !" DRV-LINE
   s"       then" DRV-LINE
   s"    loop" DRV-LINE
   s"    XL-HIT @ ;" DRV-LINE
   s" : XL-CSITES ( n n -- n ) {: start:n clen:n :}" DRV-LINE
   s"    0 XL-CS !" DRV-LINE
   s"    AOT-CSITE-N @ 0 ?do" DRV-LINE
   s"       AOT-DSITE-N @ i + 4 * AOT-DSITE-BUF@ + ACAP-W32@ {: off:n :}" DRV-LINE
   s"       off start >= off start clen + < and if 1 XL-CS +! then" DRV-LINE
   s"    loop  XL-CS @ ;" DRV-LINE
   s" : XL-CHECK ( -- )" DRV-LINE
   S\"    s\" AWB-XL-REPORT\" AWB-REC-BY-NAME {: k:n :}" DRV-LINE
   s"    k 0 < if" DRV-LINE
   S\"       s\" aot-wid-build: xtlit fixture record not found\" 74 die then" DRV-LINE
   s"    k ACAP-REC-DST {: v:ptr :}" DRV-LINE
   s"    v ACAP-W32@ {: start:n :}  v 8 + ACAP-W32@ {: clen:n :}" DRV-LINE
   s"    start clen XL-ROW-IN {: xi:n :}" DRV-LINE
   s"    xi 0 < if" DRV-LINE
   S\"       s\" aot-wid-build: xtlit body made no named code row\" 74 die then" DRV-LINE
   s"    xi XL-ROW 4 + ACAP-W32@ XL-HH0$ XL-POOL= 0= if" DRV-LINE
   S\"       s\" aot-wid-build: xtlit row names some other word\" 74 die then" DRV-LINE
   s"    start clen XL-CSITES {: cs:n :}" DRV-LINE
   s"    cs 0= 0= if" DRV-LINE
   S\"       s\" aot-wid-build: xtlit body still carries a rebased code site\" 74 die then" DRV-LINE
   S\"    s\" aot-wid-build: xtlit \" type xi XL-ROW ACAP-W32@ . cr" DRV-LINE
   S\"    s\" aot-wid-build: xtlit-csites \" type cs . cr ;" DRV-LINE
   s" XL-CHECK" DRV-LINE
   s" ;package" DRV-LINE ;

: XL-FIXTURE-LINES ( -- )
   \ Only diagnostic formatting observes the execution token's machine cell.
   s" TRUSTED: AWB-XL>BITS ( [ -- ptr a ] -- n ) ;" DRV-LINE
   S\" : AWB-XL-REPORT ( -- ) s\" awb-xl=\" type ['] HH0 AWB-XL>BITS . cr ;" DRV-LINE
   RECAPTURE-LINE
   XL-CHECK-DEF
   REPL-BOOTRUN-LINES
   S\" s\" AWB-XL-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ --- the pre-window DATA literal fixture (dot habu-aot-pre-window-0b01043c) -----
\ THE SHAPE THE WHOLE DOT IS ABOUT. A window word names a data word the PREFIX
\ defined. The prefix's `create` sits below the window's DATA span, so the address
\ its body pushes is one the window cannot describe: rebasing it by the window
\ delta is wrong (the metabuild host recompiles the whole core prefix a second
\ time without rewinding DP, so its prefix band has no counterpart in the target)
\ and leaving it is the building host's address baked into bin/hb.
\
\ HH0 is that prefix word: `create HH0 $6a09e667 , ...` in src/core/sha256.f,
\ which the cold prefix loads before the capture window opens. It is
\ short enough for habu2.f C-CALL to copy inline, so before the decline landed the
\ copy carried the chain into the window and the capture died named
\ ("recorded address site ... in neither the window's DATA span nor its code
\ span", exit 74). Its name is in the cold prefix the image bakes, which is what
\ makes the BL the decline emits relocatable at boot - and its first cell is
\ INITIALISED to the SHA-256 seed constant $6a09e667, so the boot half in
\ test/aot-data-span-forge.f gets an answer that can only come from the right
\ address in the built engine, not the zero an unrelocated read gives.
\
\ WHAT THE CHECK ASSERTS, and why it is structural rather than "the build did not
\ die": it finds AWB-PRE-READ's own captured dict record BY NAME, takes its blob
\ span from that record, and then requires BOTH halves over that span - no DATA
\ relocation site inside it (the chain is gone) AND a call site inside it whose
\ pooled callee name is HH0 (the BL is there, by name). Either half alone would
\ pass on a fixture that stopped referencing the prefix word at all.
: PREWIN-CHECK-DEF ( -- )
   BODY-CHECK-OPEN
   s" variable PW-DS  variable PW-CS" DRV-LINE
   s" : PW-DSITES ( n n -- n ) {: start:n clen:n :}" DRV-LINE
   s"    0 PW-DS !" DRV-LINE
   s"    AOT-DSITE-N @ 0 ?do" DRV-LINE
   s"       AOT-DSITE-BUF@ i 4 * + ACAP-W32@ {: off:n :}" DRV-LINE
   s"       off start >= off start clen + < and if 1 PW-DS +! then" DRV-LINE
   s"    loop  PW-DS @ ;" DRV-LINE
   s" : PW-CALLS ( n n ptr u8 n -- n ) {: start:n clen:n a:ptr u:n :}" DRV-LINE
   s"    0 PW-CS !" DRV-LINE
   s"    AOT-SITE-N @ 0 ?do" DRV-LINE
   s"       i ACAP-SITE-ROW {: r:ptr :}" DRV-LINE
   s"       r ACAP-W32@ {: off:n :}  r 4 + ACAP-W32@ {: noff:n :}" DRV-LINE
   s"       off start >= off start clen + < and" DRV-LINE
   s"       AOT-NAMES-BUF@ noff + c@ u = and if" DRV-LINE
   s"          AOT-NAMES-BUF@ noff 1+ + a u AWB-NAME= if 1 PW-CS +! then" DRV-LINE
   s"       then" DRV-LINE
   s"    loop  PW-CS @ ;" DRV-LINE
   s" : PREWIN-CHECK ( -- )" DRV-LINE
   S\"    s\" AWB-PRE-READ\" AWB-REC-BY-NAME {: k:n :}" DRV-LINE
   s"    k 0 < if" DRV-LINE
   S\"       s\" aot-wid-build: prewin fixture record not found\" 74 die then" DRV-LINE
   s"    k ACAP-REC-DST {: v:ptr :}" DRV-LINE
   s"    v ACAP-W32@ {: start:n :}  v 8 + ACAP-W32@ {: clen:n :}" DRV-LINE
   s"    start clen PW-DSITES {: ds:n :}" DRV-LINE
   s"    ds 0= 0= if" DRV-LINE
   S\"       s\" aot-wid-build: prewin body still carries a DATA site\" 74 die then" DRV-LINE
   S\"    start clen s\" HH0\" PW-CALLS {: cs:n :}" DRV-LINE
   s"    cs 0= if" DRV-LINE
   S\"       s\" aot-wid-build: prewin body holds no call to the prefix word\" 74 die then" DRV-LINE
   S\"    s\" aot-wid-build: prewin-calls \" type cs . cr" DRV-LINE
   S\"    s\" aot-wid-build: prewin-dsites \" type ds . cr ;" DRV-LINE
   s" PREWIN-CHECK" DRV-LINE
   s" ;package" DRV-LINE ;

: PREWIN-FIXTURE-LINES ( -- )
   s" : AWB-PRE-READ ( -- n ) HH0 @ ;" DRV-LINE
   S\" : AWB-PRE-REPORT ( -- ) s\" awb-pre=\" type AWB-PRE-READ . cr ;" DRV-LINE
   RECAPTURE-LINE
   PREWIN-CHECK-DEF
   REPL-BOOTRUN-LINES
   S\" s\" AWB-PRE-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ --- the AOT boot-gate fixture (dot habu-return-the-record-9c9b1731) -----------
\ THE GATE THIS REACHES. The seed resolves every baked name in the engine it is
\ booting and then rewrites a call immediate, writes an xt into a code literal,
\ or branches to the word. LAOTWIDGATE stands between the lookup and all three,
\ and it decides in layers: a wordlist THIS SEED created is admitted first, then
\ the two engine-reserved ones, then any unprotected one, and a SEALED wordlist
\ the engine already had is admitted only when it is a package's PUBLIC slot.
\ Nothing else in the tree reaches that routine, so without this fixture the last
\ layer can be deleted and every suite stays green.
\
\ WHY THE CALLEE IS A PREFIX WORD AND THE PATH IS A CALL SITE. A package the
\ fixture opens inside the capture window is a wordlist the SEED creates, so the
\ first layer answers for it and the sealed/unsealed question is never asked -
\ any in-window fixture, protected or not, simply boots. The wordlist under test
\ therefore has to be one the target engine ALREADY HAD, and the only ones a
\ fixture can reach are its own prefix's. So the window compiles a word that
\ CALLS a qualified prefix word: the capture stores that site with the qualified
\ marker, and at boot LFIND resolves it through the package row's public slot -
\ the wid the lookup actually used, which is what the gate asks about.
\ The word is compiled and never run. The gate decides at SEED time, on the call
\ site, before any of the window's code executes; running the callee would only
\ add its side effects to the fixture.
\
\ THE TWO CALLEES DIFFER IN ONE FACT, THE SEAL, and the build asserts that fact
\ rather than trusting it (GATE-SEAL-CHECK-LINES): CODE-RECLAIM seals both its
\ word-lists in src/habu/xref.f, CHECKER-TAPE seals neither in src/core/checker.f,
\ and both publish a word whose body reads a PRE-WINDOW data cell - which is what
\ makes the compile-mode inliner decline the copy and emit the BL this fixture
\ needs. A constant would have been inlined and left no call site at all.
\
\ WHY THERE IS A CONTROL MODE. Mode 1's callee is sealed and mode 2's is not, and
\ BOTH must boot: calling a public word of a sealed package is what checked source
\ does every day. What separates them is the mutation - delete the gate's
\ public-slot admit and mode 1 dies 84 naming FLOOR-FROM while mode 2 still boots.
\ Without mode 2 that exit would prove nothing about the bitmap, since a qualified
\ name that simply failed to resolve would look much the same from outside.
: GATE-SEAL-CHECK-LINES ( -- )
   GATE-ENV$ nip 0= if exit then
   s" : AWB-PKG-PUB ( ptr u8 n -- n ) DICT-WL:NAMESPACE search-wl ;" DRV-LINE
   s" : AWB-PROT ( n -- n ) {: w:n :}" DRV-LINE
   s"    w 6 rshift 8 * data-base PROT-BITS-OFF + + @  w 63 and rshift  1 and ;" DRV-LINE
   s" : AWB-?SEAL ( ptr u8 n n -- ) {: a:ptr u:n want:n :}" DRV-LINE
   s"    a u AWB-PKG-PUB {: w:n :}" DRV-LINE
   s"    w 0= if" DRV-LINE
   S\"       s\" aot-wid-build: the gate fixture's package is gone: \" type a u type cr" DRV-LINE
   S\"       s\" aot-wid-build: gate fixture package missing\" 74 die then" DRV-LINE
   s"    w AWB-PROT want = if exit then" DRV-LINE
   S\"    s\" aot-wid-build: gate fixture package \" type a u type" DRV-LINE
   S\"    s\" : seal status is not what the mode assumes\" type cr" DRV-LINE
   S\"    s\" aot-wid-build: gate fixture seal assumption broken\" 74 die ;" DRV-LINE
   S\" s\" CODE-RECLAIM\" 1 AWB-?SEAL" DRV-LINE
   S\" s\" CHECKER-TAPE\" 0 AWB-?SEAL" DRV-LINE ;

: GATE-ENTRY-LINES ( ptr u8 n -- ) {: mode:ptr mu:n :}
   s" package AWBGATE" DRV-LINE
   s" public" DRV-LINE
   mode mu s" 1" STR= if
      s" : AWB-GATE-CALL ( -- ) ndict@ CODE-RECLAIM:FLOOR-FROM drop ;" DRV-LINE
   else
      s" : AWB-GATE-CALL ( -- ) CHECKER-TAPE:DISARM ;" DRV-LINE
   then
   S\" : AWB-GATE-REPORT ( -- ) s\" awb-gate=open\" type cr ;" DRV-LINE ;

: GATE-FIXTURE-LINES ( ptr u8 n -- ) {: mode:ptr mu:n :}
   s" variable AWB-GATE-WID" DRV-LINE
   mode mu GATE-ENTRY-LINES
   s" get-current AWB-GATE-WID !" DRV-LINE
   s" ;package" DRV-LINE
   \ The target collision fixture allocates its own owner at this actual host
   \ ordinal before seeding. Neither process borrows the other's WIDN.
   S\" s\" awb-source-wid=\" type AWB-GATE-WID @ . cr" DRV-LINE
   mode mu s" 2" STR= 0= if
      s" AWB-GATE-WID @ prot-wid-add" DRV-LINE
   then
   RECAPTURE-LINE
   REPL-BOOTRUN-LINES
   S\" s\" AWBGATE:AWB-GATE-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

: FIXTURE-LINES ( -- )
   GATE-ENV$ {: g:ptr gu:n :}
   gu 0 > if g gu GATE-FIXTURE-LINES exit then
   BAD-ENV$ {: b:ptr bu:n :}
   bu 0 > if b bu BAD-FIXTURE-LINES exit then
   BAKE-ENV$ nip 0 > if BAKE-FIXTURE-LINES exit then
   TRAP-ENV$ nip 0 > if TRAP-FIXTURE-LINES exit then
   BIG-ENV$ nip 0 > if BIG-FIXTURE-LINES exit then
   EXT-ENV$ nip 0 > if EXT-FIXTURE-LINES exit then
   XL-ENV$ nip 0 > if XL-FIXTURE-LINES exit then
   PREWIN-ENV$ nip 0 > if PREWIN-FIXTURE-LINES exit then
   PLAIN-MODE? if PROT-FIXTURE-LINES then ;

: INJECT ( -- )
   GATE-SEAL-CHECK-LINES                \ host-side, before the window: nothing extra is captured
   CAPTURE-REPL-LINES
   SKEW-MODE-LINES
   FIXTURE-LINES
   WINDOW-ENDED @ 0= if RECAPTURE-LINE REPL-BOOTRUN-LINES then ;

: CAPTURE-PRELUDE ( -- )
   s" package AOT-WID-PRODUCER" DRV-LINE
   s" public" DRV-LINE
   s" ndict@ here variable PRE-R variable PRE-D PRE-D ! PRE-R !" DRV-LINE
   s" ;package" DRV-LINE
   s" require src/habu/layout.f" DRV-LINE
   s" require src/habu/aot-arm.f" DRV-LINE
   s" require src/arch/arm64/asm.f" DRV-LINE
   s" require src/arch/arm64/icode.f" DRV-LINE
   s" require src/habu/aot-decl.f" DRV-LINE
   s" require src/habu/aot-capture.f" DRV-LINE
   s" require src/habu/aot-ident.f" DRV-LINE
   s" require src/habu/fdio.f" DRV-LINE
   s" require src/habu/aot-file.f" DRV-LINE
   s" AOT-WID-PRODUCER:PRE-R @ AOT-WID-PRODUCER:PRE-D @ AOT-CAPTURE:PRELUDE-MARK" DRV-LINE ;

: ARTIFACT-WRITE ( -- )
   s" package AOT-WID-PRODUCER" DRV-LINE
   s" create KEY 32 allot" DRV-LINE
   s" create FSHA-CTX SHA256-FILE-CTX-BYTES allot" DRV-LINE
   s" : WRITE ( -- )" DRV-LINE
   s"    AOT-IDENT:RESET" DRV-LINE
   HB-TARGET-LINUX? if
      S\"    s\" src/os/linux/repl-term.f\" AOT-IDENT:PATH+"
   else
      S\"    s\" src/os/macos/repl-term.f\" AOT-IDENT:PATH+"
   then DRV-LINE
   S\"    s\" src/habu/repl.f\" AOT-IDENT:PATH+" DRV-LINE
   S\"    s\" src/habu/debug-watch.f\" AOT-IDENT:PATH+" DRV-LINE
   S\"    s\" src/habu/stepper.f\" AOT-IDENT:PATH+" DRV-LINE
   S\"    s\" src/habu/debug.f\" AOT-IDENT:PATH+" DRV-LINE
   s"    FSHA-CTX 1 SCRIPT-ARGV$ KEY SHA256-FILE-IN 0<> if 79 throw then" DRV-LINE
   s"    KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE ;" DRV-LINE
   s" WRITE" DRV-LINE
   s" ;package" DRV-LINE ;

: GEN-DRIVER ( -- )
   DRV-PATH!
   DRV-RESET
   0 WINDOW-ENDED !
   CAPTURE-PRELUDE
   INJECT
   ARTIFACT-WRITE
   DRV-IMPORT-CHECK
   DRV-PATH$ DRV-BUF DRV-U @ WRITE-ALL ;

: FORGE? ( -- bool )
   SPAN-ENV$ nip 0 >
   s" HABU_AOT_WID_SKEW" GETENV nip 0 > or
   s" HABU_AOT_WID_SPAN" GETENV nip 0 > or ;

: GEN-PATCH ( -- )
   FORGE? 0= if exit then
   DRV-RESET
   s" package AOT-WID-IMAGE-PATCH" DRV-LINE
   s" : PATCH-CELL ( n n label -- ) {: value:n old:n lab:label :}" DRV-LINE
   s"    lab LBL-BOUND? 0= if 79 throw then" DRV-LINE
   s"    lab LABEL>N cells LBLP + @ 4 * {: off:n :}" DRV-LINE
   s"    off 0< off CODELEN @ 8 - > or if 79 throw then" DRV-LINE
   s"    CODE off + {: dst:ptr :}" DRV-LINE
   s"    dst FS-U64@ old <> if 79 throw then" DRV-LINE
   s"    8 0 ?do value i 8 * rshift dst i + c! loop ;" DRV-LINE
   s" : RUN ( -- )" DRV-LINE
   SPAN-FORGE-LINE WID-FORGE-LINE
   S\"    s\" hb\" 0 SCRIPT-ARGV$ DRV-EMIT-IMAGE" DRV-LINE
   s"    0 SCRIPT-ARGV$ CODESIGN:ENSURE ;" DRV-LINE
   s" RUN" DRV-LINE
   s" ;package" DRV-LINE
   PATCH$ DRV-BUF DRV-U @ WRITE-ALL ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG ;

: CHILD ( ptr u8 n -- )
   >LEN s" " >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc 0<> if s" aot-wid-build: child failed" rc die then ;

\ The cold host depends on nothing this builder varies, so every mode - and
\ every other fixture - takes the one test/cold-engine.f emits per tree.
: BUILD-COLD ( -- )
   COLD$ COLD-ENGINE:PROVIDE ;

: CAPTURE-FIXTURE ( -- )
   ARGS DRV-PATH$ ARG s" --" ARG ART$ ARG COLD$ ARG
   COLD$ CHILD ;

: WRITE-FIXTURE ( -- )
   ARGS s" test/native-fixture-write.f" ARG
   FORGE? if PATCH$ ARG then
   s" --" ARG IMAGE$ ARG ART$ ARG COLD$ ARG
   ENGINE-CANDIDATE:PATH$ CHILD ;

public

: BUILD ( -- )
   PATHS
   GEN-DRIVER
   GEN-PATCH
   BUILD-COLD
   CAPTURE-FIXTURE
   WRITE-FIXTURE
   s" aot-wid-build: hb-pwid ready" type cr ;

;package

AOT-WID-BUILD:BUILD

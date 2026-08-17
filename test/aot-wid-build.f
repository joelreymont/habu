\ aot-wid-build.f - build a protected-WID variant of the stdin engine.
\
\ Run as `bin/hb --load test/aot-wid-build.f` with HB_TMP pointing at a private
\ directory; on success it writes an `hb-pwid` engine into that directory. The
\ variant is identical to the shipped `bin/hb` except that its ahead-of-time
\ (AOT) section carries a protected-WID bitmap with two extra wordlist ids set
\ (300 and 8000) on top of whatever the metabuild host itself protects. Nothing
\ in production is touched: the extra bits are set ONLY in this throwaway
\ variant, through the same capture-buffer word the real metabuild uses
\ (aot-capture.f ACAP-PWID-SET), so the shipped engine bakes exactly the band
\ its own build produced.
\
\ How the bits are injected without editing production source: the stdin
\ metabuild driver src/habu/stdin.f ends with a single top-level
\ `STDIN-DRIVER:RUN` call, and everything before it is what a generated driver
\ keeps. WHICH BYTES THOSE ARE IS NOT THIS FILE'S TO SAY - it asks
\ BUILD-FIXPOINT's BF-DRV-SOURCE-KEEP, the same word the production chain driver
\ asks, so the tail token and its fail-closed check live in one place instead of
\ two that drift. This builder then appends the driver's own terminal sequence
\ written out at interpret level, with the protected-WID work spliced in between
\ capturing the REPL and emitting the image. CAPTURE-REPL and the capture words
\ are package-private, so the appended text reopens STDIN-DRIVER and AOT-CAPTURE
\ to reach them by their bare names - it never adds a public tail to either. The
\ rest of the build reuses tools/build-fixpoint.f exactly as the normal stdin
\ build does.
\
\ Sixteen modes, one per entry below (HABU_AOT_GATE serves two), selected by
\ environment so one builder serves every case its companion suites need -
\ test/aot-wid-suite.f, test/aot-wide-format-suite.f and the PTY half in
\ test/aot-data-span-forge.f:
\
\   (default)          bake the two fixture ids, after checking the capture's own
\                      shape contract on the live host: the band carries
\                      PROT-REG-TAG, the captured buffer is a bit-for-bit image
\                      of the live band, and the table-era conversion leg accepts
\                      an empty registry without setting a bit.
\   HABU_PWID_OOR=N    hand N to ACAP-PWID-SET instead. The capture must refuse
\                      any id at or above the bitmap's bound rather than write
\                      outside the band, so the build dies named and no engine
\                      appears.
\   HABU_PWID_LEGACY_N=N  hand N to ACAP-PWID-LEGACY as a table-era row count.
\                      A count outside [0, PROT-WID-LEGACY-MAX] is not a legacy
\                      registry at all, so the build dies named rather than
\                      walking the read loop out of the band.
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
\   HABU_AOT_GATE_WID=N  land the gate fixture's package on wordlist id N in the
\                      METABUILD HOST, so the case is built on an id the target
\                      engine already uses (see GATE-WID-LINES).
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
\                      reporter ABOVE them, and take the lot in with a widened
\                      re-capture. The build then asserts the three offsets that
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
require lib/vector.f
require lib/fmt.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/date.f
require tools/build-fixpoint.f

package AOT-WID-BUILD
using BUILD-FIXPOINT

\ Driver = the stdin source BUILD-FIXPOINT keeps + this file's injection. The
\ big-window mode writes BIG-FILLER-N definitions into it, which is what sets this
\ cap rather than the ~4 KB source.
$10000 constant DRV-CAP

create DRV-BUF DRV-CAP allot   variable DRV-U
create DRV-PATH-BUF FS-PATH-CAP allot   variable DRV-PATH-U

: DRV-PATH$ ( -- ptr u8 n )
   DRV-PATH-BUF DRV-PATH-U @ ;

: DRV-PATH! ( -- )                 \ <HB_TMP>/pwid-driver.f
   BF-TMP$ s" pwid-driver.f" DRV-PATH-BUF JOIN-PATH DRV-PATH-U ! ;

: DRV-RESET ( -- )
   0 DRV-U ! ;

: DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DRV-U @ u + DRV-CAP > if s" aot-wid-build: driver buffer overflow" BF-BUILD-RC die then
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
   <> if s" aot-wid-build: a generated AOT-CAPTURE block has no `using AOT-BUF`" BF-BUILD-RC die then ;

create DRV-CH 1 allot
: DRV-CH+ ( n -- ) {: c:n :}
   c DRV-CH c!  DRV-CH 1 DRV+ ;
: DRV-U+ ( n -- ) {: v:n :}        \ decimal, for the generated filler names
   v 10 >= if v 10 / recurse then
   v 10 mod 48 + DRV-CH+ ;

\ Optional AOT DATA-span forge (dot habu-guard-aot-data-49de2ee6). The reserve in
\ EM-AOT-RELOC-DATA advances DP by the baked LAOTDATASIZE span read straight from
\ the image; test/aot-data-span-forge.f probes that guard by baking a forged span.
\ When HABU_AOT_SPAN is set to a decimal, that value overwrites the captured span
\ AFTER CAPTURE-REPL and BEFORE ENGINE-EMIT:FORTH, so LAOTDATASIZE carries the forged value
\ (the forge test passes 2*DATA-SIZE, unambiguously past the seed headroom). No env
\ leaves the real capture untouched (the plain protected-WID build path).
: SPAN-FORGE-LINE ( -- )
   s" HABU_AOT_SPAN" GETENV {: v:ptr vu:n :}
   vu 0 > if
      v vu DRV+  s"  AOT-DATA-SIZE !" DRV-LINE
   then ;

\ Optional wid-window forges (dot habu-rebase-captured-wids-54dec421). The seed
\ rebases every captured wid through the baked window, and refuses one the window
\ does not contain - on either side. The capture refuses such a record at capture
\ time, so the only way to bake one is to move the window AFTER the capture and
\ before the emit. Raising the base puts every captured wid BELOW the window;
\ shrinking the span puts them past its end. Both need a fixture that captures a
\ non-zero wid at all - the REPL sources define no package, so HABU_AOT_GATE
\ supplies the package these are combined with.
: WID-FORGE-LINE ( -- )
   s" HABU_AOT_WID_SKEW" GETENV {: v:ptr vu:n :}
   vu 0 > if
      s" AOT-BUF:AOT-WID-W0 @ " DRV+  v vu DRV+
      s"  + AOT-BUF:AOT-WID-W0 !" DRV-LINE
   then
   s" HABU_AOT_WID_SPAN" GETENV {: p:ptr pu:n :}
   pu 0 > if
      p pu DRV+  s"  AOT-BUF:AOT-WID-SPAN !" DRV-LINE
   then ;

\ --- the fixture contract -----------------------------------------------------
\ These two ids are what test/aot-wid-suite.f probes for in the built engine, so
\ any drift here turns that suite red (it is self-checking) - keep the two in
\ step. 300 is above the u8 ceiling the old u32-row table existed to clear; 8000
\ sits high in the band, far above any wordlist a boot allocates, so the suite can
\ also assert that its NEIGHBOUR 8001 came back unprotected - a restore that
\ smeared or mis-shifted the band would set it.
: FIXTURE-A$ ( -- ptr u8 n ) s" 300" ;
: FIXTURE-B$ ( -- ptr u8 n ) s" 8000" ;

\ --- shape and conversion checks emitted into the driver ----------------------
\ These run in the METABUILD HOST, where the live band and the capture buffer both
\ exist, and they use the very words the real capture uses. They are the only
\ place the capture's format contract can be checked against a live band. Those
\ words are private to package AOT-CAPTURE, so the definitions are emitted inside
\ a reopened block of it and stay private too.
: SHAPE-CHECK-DEF ( -- )
   DRV-AOT-CAPTURE
   s" : PWID-SHAPE-CHECK ( -- )" DRV-LINE
   s"    ACAP-PWID-TAG@ PROT-REG-TAG <> if" DRV-LINE
   S\"       s\" aot-wid-build: metabuild host band carries no bitmap tag\" 74 die then" DRV-LINE
   s"    PROT-BITS-BYTES 0 ?do" DRV-LINE
   s"       AOT-LIVE-DATA PROT-BITS-OFF + i + AOT-A>U8 c@  AOT-PWID-BUF@ i + c@ <> if" DRV-LINE
   S\"          s\" aot-wid-build: capture is not a bit-for-bit image of the live band\" 74 die" DRV-LINE
   s"       then" DRV-LINE
   s"    loop ;" DRV-LINE ;

\ The table-era leg converts u32 rows read from a FIXED live address into bits, so
\ it cannot be handed a fabricated table: on a bitmap-era host that address holds
\ the bitmap, and reading it as rows yields whatever ids the host's own bits happen
\ to spell. What CAN be checked here without forging live memory is the empty
\ table-era registry - count 0, which is exactly the shape every shipped
\ table-era engine carried, and the shape the real changeover fed this leg. It must
\ be accepted (not mistaken for an unknown lineage) and must leave no bit set.
\ Its opposite, a count that is not a legacy registry at all, is the
\ HABU_PWID_LEGACY_N refusal build.
\
\ NOT covered here, and recorded as a gap rather than faked: the row->bit mapping
\ for a NON-empty table. It needs a table-era host, which no longer exists; its
\ only evidence is the one-time changeover measurement (an old-table host and a
\ new-bitmap host both building bin/hb to the same bytes). That is a reason to
\ retire the leg, not to keep it untested - dot habu-retire-the-legacy-31ad57bc.
: LEGACY-CHECK-DEF ( -- )
   s" : PWID-LEGACY-CHECK ( -- )" DRV-LINE
   s"    ACAP-PWID-CLEAR" DRV-LINE
   s"    0 ACAP-PWID-LEGACY" DRV-LINE
   s"    ACAP-PWID-COUNT 0 <> if" DRV-LINE
   S\"       s\" aot-wid-build: empty legacy registry set a bit\" 74 die then" DRV-LINE
   s"    ACAP-PWID-CAPTURE ;" DRV-LINE
   s" ;package" DRV-LINE ;

\ --- the body between CAPTURE-REPL and the image emit --------------------------
: OOR-ENV$ ( -- ptr u8 n )       s" HABU_PWID_OOR" GETENV ;
: LEGACY-ENV$ ( -- ptr u8 n )    s" HABU_PWID_LEGACY_N" GETENV ;
: SKEW-ENV$ ( -- ptr u8 n )      s" HABU_AOT_D0_SKEW" GETENV ;
: BAKE-ENV$ ( -- ptr u8 n )      s" HABU_AOT_BAKE" GETENV ;
: TRAP-ENV$ ( -- ptr u8 n )      s" HABU_AOT_TRAP" GETENV ;
: BIG-ENV$ ( -- ptr u8 n )       s" HABU_AOT_BIG" GETENV ;
: EXT-ENV$ ( -- ptr u8 n )       s" HABU_AOT_EXT" GETENV ;
: XL-ENV$ ( -- ptr u8 n )        s" HABU_AOT_XTLIT" GETENV ;
: PREWIN-ENV$ ( -- ptr u8 n )    s" HABU_AOT_PREWIN" GETENV ;
: GATE-ENV$ ( -- ptr u8 n )      s" HABU_AOT_GATE" GETENV ;

: REFUSE-BODY ( ptr u8 n ptr u8 n -- ) {: v:ptr vu:n w:ptr wu:n :}
   v vu DRV+  s"  " DRV+  w wu DRV-LINE ;

: CHECKS-WANTED? ( -- bool )       \ only the plain fixture build carries them
   OOR-ENV$ nip 0 =  LEGACY-ENV$ nip 0 =  and  SKEW-ENV$ nip 0 =  and
   BAKE-ENV$ nip 0 =  and  TRAP-ENV$ nip 0 =  and  BIG-ENV$ nip 0 =  and
   EXT-ENV$ nip 0 =  and  XL-ENV$ nip 0 =  and
   PREWIN-ENV$ nip 0 =  and  GATE-ENV$ nip 0 =  and ;

\ The two shape checks are only DEFINED for the plain fixture build
\ (CHECKS-WANTED?), so the window-content modes, which reuse the two bits and
\ nothing else, must not call them.
: FIXTURE-CHECK-LINES ( -- )
   s" PWID-SHAPE-CHECK" DRV-LINE
   s" PWID-LEGACY-CHECK" DRV-LINE ;

: FIXTURE-BODY ( -- )
   CHECKS-WANTED? if FIXTURE-CHECK-LINES then
   FIXTURE-A$ DRV+  s"  ACAP-PWID-SET" DRV-LINE
   FIXTURE-B$ DRV+  s"  ACAP-PWID-SET" DRV-LINE ;

\ Re-run the real capture over the real window with the DATA span start moved.
\ AOT-ARM's window cells still hold the span CAPTURE-REPL just latched, so raising
\ D0 and handing the same WINDOW$ to the production entry point (AOT-CAPTURE's
\ public CAPTURE) gives a window that cannot describe its own contents - a poked
\ wrong value, not a stand-in. The line is emitted inside the reopened
\ AOT-CAPTURE package, where CAPTURE resolves bare.
: SKEW-BODY ( ptr u8 n -- ) {: v:ptr vu:n :}
   s" AOT-ARM:D0 @ " DRV+  v vu DRV+  s"  + AOT-ARM:D0 !" DRV-LINE
   s" AOT-ARM:WINDOW$ CAPTURE" DRV-LINE ;

: PWID-BODY ( -- )
   OOR-ENV$ {: o:ptr ou:n :}
   ou 0 > if o ou s" ACAP-PWID-SET" REFUSE-BODY exit then
   LEGACY-ENV$ {: l:ptr lu:n :}
   lu 0 > if l lu s" ACAP-PWID-LEGACY" REFUSE-BODY exit then
   SKEW-ENV$ {: k:ptr ku:n :}
   ku 0 > if k ku SKEW-BODY exit then
   FIXTURE-BODY ;

\ CAPTURE-REPL is private to package STDIN-DRIVER, so the appended text reopens
\ that package, ticks it, closes the package and executes the token - reaching the
\ real word by its real name without publishing anything.
: CAPTURE-REPL-LINES ( -- )
   s" package STDIN-DRIVER" DRV-LINE
   s" ' CAPTURE-REPL" DRV-LINE
   s" ;package" DRV-LINE
   s" execute" DRV-LINE ;

\ Append the checks the fixture build needs, then stdin.f's own terminal sequence
\ with the protected-WID work spliced in after CAPTURE-REPL.
\ --- the two window-content fixtures ------------------------------------------
\ Both work the way SKEW-BODY does: define at top level, then hand the REAL entry
\ point (AOT-CAPTURE's public CAPTURE) a window widened to take in what was just
\ defined. stdin.f's own window variables supply the START of every span, so the
\ blob, record and DATA spans all still begin exactly where CAPTURE-REPL began
\ them; only the ends move out to the live cursors. Nothing about the capture, the
\ bake or the seed is stood in for.
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

\ WINDOW-CLOSE moves the window's three END cursors to the live ones and re-reads
\ WIDN, leaving its START where CAPTURE-REPL put it - which is exactly the widened
\ window these fixtures need, and one word instead of six cursor reads.
: RECAPTURE-LINE ( -- )
   s" AOT-ARM:WINDOW-CLOSE" DRV-LINE
   WID-NARROW-LINE
   s" AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE" DRV-LINE ;

: REPL-BOOTRUN-LINES ( -- )
   S\" s\" INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE
   S\" s\" BPW-INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE
   S\" s\" S-INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ An initialised cell plus a word that reads it. The word's reference to the cell
\ is a DATA address literal in the widened window, so the value only reaches the
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
\ several times that, with the three things that have to survive it defined ABOVE
\ the filler: a data cell, a callee too long for the inliner to copy (habu2.f
\ INL-MAX), and a reporter that calls the callee and prints the cell. So the
\ engine can only report the magic if a call site AND a DATA site whose blob
\ offsets do not fit sixteen bits were both recorded and patched at boot.
\ The filler words carry no calls of their own; they exist to push the three
\ words that matter past the old ceiling.
200 constant BIG-FILLER-N          \ ~454 blob bytes each: 200 puts the window near 109 KB

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
   S\" : AWB-XL-REPORT ( -- ) s\" awb-xl=\" type ['] HH0 . cr ;" DRV-LINE
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
\ which PFX-LOAD-CORE-FILES loads long before CAPTURE-REPL opens the window. It is
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
\ public-slot admit and mode 1 dies 84 naming WATCHERS while mode 2 still boots.
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
      s" : AWB-GATE-CALL ( -- ) CODE-RECLAIM:WATCHERS drop ;" DRV-LINE
   else
      s" : AWB-GATE-CALL ( -- ) CHECKER-TAPE:HOLD-DISARM ;" DRV-LINE
   then
   S\" : AWB-GATE-REPORT ( -- ) s\" awb-gate=open\" type cr ;" DRV-LINE ;

\ --- landing the fixture on a chosen wordlist id -------------------------------
\ HABU_AOT_GATE_WID names the id the fixture's package must get IN THE HOST, so a
\ case can be built on an id the TARGET engine already uses - the alias the wid
\ rebase exists to make harmless (dot habu-rebase-captured-wids-54dec421). The
\ suite chooses it from the target's own band and its own WIDN; nothing is
\ written down here. Burning only moves forward, so a host already past the id
\ refuses BY NAME rather than quietly testing a different one.
: GATE-WID-ENV$ ( -- ptr u8 n ) s" HABU_AOT_GATE_WID" GETENV ;

: GATE-WID-LINES ( -- )
   GATE-WID-ENV$ {: w:ptr wu:n :}
   wu 0= if exit then
   s" : AWB-WID-WANT ( -- n ) " DRV+  w wu DRV+  s"  ;" DRV-LINE
   \ The burn reads WIDN through a primitive and a layout constant, never through
   \ a host word: this text is compiled INSIDE the capture window, so a call to
   \ anything the metabuild host has and the target has not bakes a name the seed
   \ cannot resolve (measured: exit 81, the boot BL-range assertion).
   s" : AWB-BURN ( -- ) begin data-base WIDN-CELL + @ AWB-WID-WANT < while wordlist drop repeat ;" DRV-LINE
   s" AWB-BURN" DRV-LINE ;

: GATE-WID-CHECK-LINES ( -- )
   GATE-WID-ENV$ nip 0= if exit then
   s" : AWB-WID-CHECK ( -- )" DRV-LINE
   s"    AWB-GATE-WID @ AWB-WID-WANT = if exit then" DRV-LINE
   S\"    s\" aot-wid-build: the fixture did not land on the wordlist id it was given\" type cr" DRV-LINE
   S\"    s\" aot-wid-build: host wordlist id already past the alias target\" 74 die ;" DRV-LINE
   s" AWB-WID-CHECK" DRV-LINE ;

: GATE-FIXTURE-LINES ( ptr u8 n -- ) {: mode:ptr mu:n :}
   s" variable AWB-GATE-WID" DRV-LINE
   GATE-WID-LINES
   mode mu GATE-ENTRY-LINES
   s" get-current AWB-GATE-WID !" DRV-LINE
   s" ;package" DRV-LINE
   GATE-WID-CHECK-LINES
   mode mu s" 2" STR= 0= if
      s" AWB-GATE-WID @ prot-wid-add" DRV-LINE
   then
   RECAPTURE-LINE
   REPL-BOOTRUN-LINES
   S\" s\" AWBGATE:AWB-GATE-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

: FIXTURE-LINES ( -- )
   GATE-ENV$ {: g:ptr gu:n :}
   gu 0 > if g gu GATE-FIXTURE-LINES exit then
   BAKE-ENV$ nip 0 > if BAKE-FIXTURE-LINES exit then
   TRAP-ENV$ nip 0 > if TRAP-FIXTURE-LINES exit then
   BIG-ENV$ nip 0 > if BIG-FIXTURE-LINES exit then
   EXT-ENV$ nip 0 > if EXT-FIXTURE-LINES exit then
   XL-ENV$ nip 0 > if XL-FIXTURE-LINES exit then
   PREWIN-ENV$ nip 0 > if PREWIN-FIXTURE-LINES then ;

: INJECT ( -- )
   CHECKS-WANTED? if
      SHAPE-CHECK-DEF
      LEGACY-CHECK-DEF
   then
   GATE-SEAL-CHECK-LINES                \ host-side, before the window: nothing extra is captured
   CAPTURE-REPL-LINES
   DRV-AOT-CAPTURE
   PWID-BODY
   s" ;package" DRV-LINE
   FIXTURE-LINES
   SPAN-FORGE-LINE
   WID-FORGE-LINE
   s" 0 0= STDIN? !" DRV-LINE
   s" HB@ 0 ENGINE-EMIT:FORTH" DRV-LINE
   S\" s\" hb\" STDIN-OUT DRV-EMIT-IMAGE" DRV-LINE
   s" DRV-EXIT-OK" DRV-LINE ;

: GEN-DRIVER ( -- )
   DRV-PATH!
   BF-DRV-SOURCE-KEEP {: keep:n :}  \ the build's own split of the stdin driver
   DRV-RESET
   BF-SOURCE-BUF keep DRV+          \ stdin.f minus its terminal driver call
   INJECT                           \ ... plus the bitmap-working fixture
   DRV-IMPORT-CHECK
   DRV-PATH$ DRV-BUF DRV-U @ WRITE-ALL ;

: EMIT-PWID-STDIN ( -- )
   s" stage2-src" DRV-PATH$ BF-EMIT-STDIN-RUN-SOURCE ;

\ Build the maker (hb-pwid-mk) from the stage engine, then run the maker to emit
\ the final variant hb-pwid (mirrors BF-BUILD-STDIN-FROM-STAGE with our driver).
: RUN-MAKER ( -- )
   s" stage2-got" s" hb-pwid-mk" BF-RENAME-TMP
   s" hb-pwid-mk" BF-CHMOD-X-TMP
   s" hb-stdin-got" BF-REMOVE-TMP
   s" hb-pwid-mk" BF-RUN-ENV-TMP BF-RC0
   s" hb-stdin-got" BF-EXPECT
   s" hb-stdin-got" s" hb-pwid" BF-RENAME-TMP
   s" hb-pwid" BF-CHMOD-X-TMP
   s" hb-pwid" BF-CODESIGN-VERIFY-TMP ;

public

: BUILD ( -- )
   BF-STAGE-FIXPOINT               \ stage engine at fixpoint (reused if bin/hb already converged)
   GEN-DRIVER
   EMIT-PWID-STDIN
   BF-CERTIFY-STDIN
   BF-RUN-STAGE                    \ stage compiles the injected stdin source -> maker
   RUN-MAKER
   s" aot-wid-build: hb-pwid ready" type cr ;

;package

AOT-WID-BUILD:BUILD

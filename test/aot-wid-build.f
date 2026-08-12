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
\ `STDIN-DRIVER:RUN` call. This builder reads that file, verifies it still ends
\ with that call (and dies with a clear message if the tail ever drifts), drops
\ the call, and appends the same sequence written out at interpret level, with
\ the protected-WID work spliced in between capturing the REPL and emitting the
\ image. CAPTURE-REPL and the capture words are package-private, so the appended
\ text reopens STDIN-DRIVER and AOT-CAPTURE to reach them by their bare names -
\ it never adds a public tail to either. The rest of the build reuses
\ tools/build-fixpoint.f exactly as the normal stdin build does.
\
\ Nine modes, selected by environment so one builder serves every case its
\ companion suites need - test/aot-wid-suite.f, test/aot-wide-format-suite.f and
\ the PTY half in test/aot-data-span-forge.f:
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

: STDIN-SRC-PATH ( -- ptr u8 n ) s" src/habu/stdin.f" ;

$4000 constant SRC-CAP             \ src/habu/stdin.f is ~4 KB
\ Driver = source (minus GO) + injection. The big-window mode writes BIG-FILLER-N
\ definitions into it, which is what sets this cap rather than the ~4 KB source.
$10000 constant DRV-CAP

create SRC-BUF SRC-CAP allot   variable SRC-U
create DRV-BUF DRV-CAP allot   variable DRV-U
create DRV-PATH-BUF FS-PATH-CAP allot   variable DRV-PATH-U

: DRV-PATH$ ( -- ptr u8 n )
   DRV-PATH-BUF DRV-PATH-U @ ;

: DRV-PATH! ( -- )                 \ <HB_TMP>/pwid-driver.f
   BF-TMP$ s" pwid-driver.f" DRV-PATH-BUF JOIN-PATH DRV-PATH-U ! ;

: READ-STDIN-SRC ( -- )
   STDIN-SRC-PATH SRC-BUF SRC-CAP READ-ALL SRC-U !
   SRC-U @ 0 <= if s" aot-wid-build: cannot read src/habu/stdin.f" BF-BUILD-RC die then ;

: WS? ( n -- bool ) {: c:n :}      \ whitespace: space, tab, cr, lf
   c 32 = c 9 = or c 13 = or c 10 = or ;

: SRC-LAST ( -- n )                \ index of last non-whitespace byte, or -1
   SRC-U @ 1-
   begin dup 0 >= while
      dup SRC-BUF + c@ WS? 0= if exit then
      1-
   repeat ;

: TAIL-BAD ( -- )
   s" aot-wid-build: src/habu/stdin.f no longer ends with STDIN-DRIVER:RUN" BF-BUILD-RC die ;

: RUN-TAIL$ ( -- ptr u8 n ) s" STDIN-DRIVER:RUN" ;

\ Length of stdin.f to keep: everything up to (not including) the trailing
\ `STDIN-DRIVER:RUN`. Fail closed if the file does not end with that token.
: RUN-KEEP ( -- n )
   SRC-LAST {: l:n :}
   RUN-TAIL$ {: t:ptr tu:n :}
   l 1+ tu < if TAIL-BAD then
   l 1+ tu - SRC-BUF + tu t tu STR= 0= if TAIL-BAD then
   l 1+ tu > if l tu - SRC-BUF + c@ WS? 0= if TAIL-BAD then then
   l 1+ tu - ;

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
   s" package AOT-CAPTURE" DRV-LINE
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

: REFUSE-BODY ( ptr u8 n ptr u8 n -- ) {: v:ptr vu:n w:ptr wu:n :}
   v vu DRV+  s"  " DRV+  w wu DRV-LINE ;

: CHECKS-WANTED? ( -- bool )       \ only the plain fixture build carries them
   OOR-ENV$ nip 0 =  LEGACY-ENV$ nip 0 =  and  SKEW-ENV$ nip 0 =  and
   BAKE-ENV$ nip 0 =  and  TRAP-ENV$ nip 0 =  and  BIG-ENV$ nip 0 =  and
   EXT-ENV$ nip 0 =  and ;

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
\ The window variables are stdin.f's own globals and still hold the span
\ CAPTURE-REPL just used, so this is the production entry point (AOT-CAPTURE's
\ public CAPTURE) called with a window that cannot describe its own contents -
\ not a stand-in for one. The line is emitted inside the reopened AOT-CAPTURE
\ package, where CAPTURE resolves bare.
: SKEW-BODY ( ptr u8 n -- ) {: v:ptr vu:n :}
   s" REPL-B0 @ REPL-B1 @  REPL-R0 @ REPL-R1 @" DRV+
   s"  REPL-D0 @ " DRV+  v vu DRV+  s"  +  REPL-D1 @  CAPTURE" DRV-LINE ;

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
: RECAPTURE-LINE ( -- )
   s" REPL-B0 @ cp@  REPL-R0 @ ndict@  REPL-D0 @ here  AOT-CAPTURE:CAPTURE" DRV-LINE ;

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
   s" package AOT-CAPTURE" DRV-LINE
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
   s" package AOT-CAPTURE" DRV-LINE
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

: FIXTURE-LINES ( -- )
   BAKE-ENV$ nip 0 > if BAKE-FIXTURE-LINES exit then
   TRAP-ENV$ nip 0 > if TRAP-FIXTURE-LINES exit then
   BIG-ENV$ nip 0 > if BIG-FIXTURE-LINES exit then
   EXT-ENV$ nip 0 > if EXT-FIXTURE-LINES then ;

: INJECT ( -- )
   CHECKS-WANTED? if
      SHAPE-CHECK-DEF
      LEGACY-CHECK-DEF
   then
   CAPTURE-REPL-LINES
   s" package AOT-CAPTURE" DRV-LINE
   PWID-BODY
   s" ;package" DRV-LINE
   FIXTURE-LINES
   SPAN-FORGE-LINE
   s" 0 0= STDIN? !" DRV-LINE
   s" HB@ 0 ENGINE-EMIT:FORTH" DRV-LINE
   S\" s\" hb\" STDIN-OUT DRV-EMIT-IMAGE" DRV-LINE
   s" DRV-EXIT-OK" DRV-LINE ;

: GEN-DRIVER ( -- )
   DRV-PATH!
   READ-STDIN-SRC
   RUN-KEEP {: keep:n :}
   DRV-RESET
   SRC-BUF keep DRV+                \ stdin.f minus its terminal driver call
   INJECT                           \ ... plus the bitmap-working fixture
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

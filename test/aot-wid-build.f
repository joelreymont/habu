\ aot-wid-build.f - build a protected-WID partial native image: run with TMPDIR
\ pointing at a private directory, it writes an `hb-pwid` engine there. The
\ default variant carries the REPL and two fixture packages, one protected. Each
\ HABU_AOT_* mode is described at its fixture word; test/aot-wid-suite.f and
\ test/aot-data-window-suite.f run the builder in a child and probe its hb-pwid.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/cold-engine.f
require test/fixture-writer.f

package AOT-WID-BUILD

$2000 constant DRV-CAP
$10000 constant IO-CAP
120000 constant CHILD-TIMEOUT-MS
74 constant BUILD-RC

create DRV-BUF DRV-CAP allot   variable DRV-U
create DRV-PATH-BUF PATH-CAP allot   variable DRV-PATH-U
create ROOT-BUF PATH-CAP allot variable ROOT-U
create COLD-BUF PATH-CAP allot variable COLD-U
create ART-BUF PATH-CAP allot variable ART-U
create IMAGE-BUF PATH-CAP allot variable IMAGE-U
create OUT IO-CAP allot
create ERR IO-CAP allot
variable WINDOW-ENDED

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: COLD$ ( -- ptr u8 n ) COLD-BUF COLD-U @ ;
: ART$ ( -- ptr u8 n ) ART-BUF ART-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: PATHS ( -- )
   s" TMPDIR" GETENV {: a:ptr u:n :}
   u 0= u PATH-CAP > or if
      s" aot-wid-build: TMPDIR must name a private directory" BUILD-RC die then
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ s" hb-cold" COLD-BUF FS:JOIN-PATH COLD-U !
   ROOT$ s" window.aot" ART-BUF FS:JOIN-PATH ART-U !
   ROOT$ s" hb-pwid" IMAGE-BUF FS:JOIN-PATH IMAGE-U ! ;

: DRV-PATH$ ( -- ptr u8 n )
   DRV-PATH-BUF DRV-PATH-U @ ;

: DRV-PATH! ( -- )                 \ <TMPDIR>/pwid-driver.f
   ROOT$ s" pwid-driver.f" DRV-PATH-BUF FS:JOIN-PATH DRV-PATH-U ! ;

: DRV-RESET ( -- )
   0 DRV-U ! ;

: DRV+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DRV-U @ u + DRV-CAP > if s" aot-wid-build: driver buffer overflow" BUILD-RC die then
   a DRV-BUF DRV-U @ + u BYTE-COPY
   DRV-U @ u + DRV-U ! ;

: DRV-NL ( -- ) S\" \n" DRV+ ;

: DRV-LINE ( ptr u8 n -- ) DRV+ DRV-NL ;

\ Every reopened AOT-CAPTURE block below reads the capture buffers by their bare
\ names, package AOT-BUF's public surface (src/habu/aot-decl.f). aot-capture.f's
\ own `using AOT-BUF` closes with that file's `;package`, and this text is a
\ fresh eval frame, so each generated block opens its own import.
\ DRV-IMPORT-CHECK proves no block skipped it.
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

\ HABU_AOT_SPAN=N overwrites the emitted AOT DATA span, after the ordinary
\ artifact reader and image writer accept the valid capture. The generated patch
\ checks its bound label, full cell span and original value before rebuilding
\ and signing that image.
: SPAN-FORGE-LINE ( -- )
   s" HABU_AOT_SPAN" GETENV {: v:ptr vu:n :}
   vu 0 > if
      v vu DRV+
      s"  AOT-BUF:AOT-DATA-SIZE @ LAOTDATASIZE A64ICODE:LABEL@ PATCH-CELL" DRV-LINE
   then ;

\ HABU_AOT_WID_SKEW=N raises the baked window's base by N, so records name
\ wordlists below it; HABU_AOT_WID_SPAN=N sets its span to N, so records name
\ wordlists past its end. The seed rebases every captured wid through the baked
\ window and must refuse one outside it, at boot. The capture refuses such a
\ record itself, so the emitted scalar is corrupted after the normal writer. The
\ baked base is the constant every captured offset counts from
\ (src/habu/aot-decl.f AOT-BUF:WID-REL-BASE). Both need a fixture that captures
\ a non-zero wid: the REPL sources define no package, so HABU_AOT_GATE supplies
\ one.
: WID-FORGE-LINE ( -- )
   s" HABU_AOT_WID_SKEW" GETENV {: v:ptr vu:n :}
   vu 0 > if
      s" AOT-BUF:WID-REL-BASE " DRV+  v vu DRV+
      s"  + AOT-BUF:WID-REL-BASE AOT-WINDOW:LWIDW0 A64ICODE:LABEL@ PATCH-CELL" DRV-LINE
   then
   s" HABU_AOT_WID_SPAN" GETENV {: p:ptr pu:n :}
   pu 0 > if
      p pu DRV+
      s"  AOT-BUF:AOT-WID-SPAN @ AOT-WINDOW:LWIDSPAN A64ICODE:LABEL@ PATCH-CELL" DRV-LINE
   then ;

: SKEW-ENV$ ( -- ptr u8 n )      s" HABU_AOT_D0_SKEW" GETENV ;
: SPAN-ENV$ ( -- ptr u8 n )      s" HABU_AOT_SPAN" GETENV ;
: BAKE-ENV$ ( -- ptr u8 n )      s" HABU_AOT_BAKE" GETENV ;
: TRAP-ENV$ ( -- ptr u8 n )      s" HABU_AOT_TRAP" GETENV ;
: GATE-ENV$ ( -- ptr u8 n )      s" HABU_AOT_GATE" GETENV ;

\ The plain protected-WID fixture is the fallback mode, but the D0-skew refusal
\ reaches that point with a window of its own to keep: it re-captures the closed
\ REPL window and wants no package of ours inside it.
: PLAIN-MODE? ( -- bool )
   SKEW-ENV$ nip 0 = ;

\ HABU_AOT_D0_SKEW=N reruns the real capture over the same window with its DATA
\ span start raised by N. AOT-ARM's window cells still hold the span just
\ latched, so the same WINDOW$ handed to the production CAPTURE is a window that
\ cannot describe its own contents: a poked wrong value, not a stand-in. An N
\ past the whole span leaves the window none of the address chains its blob
\ holds, which the capture must refuse rather than bake or skip, so the build
\ dies named and no engine appears. The line is emitted inside the reopened
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

\ The terminal and REPL files are INCLUDEd because the window has to compile
\ them here; the debugger is one REQUIRE because src/habu/debug.f requires the
\ watch cells and the stepper itself, and naming all three loaded each twice
\ (`duplicate definition: BPW-MAX`). The cold host provides none of the four, so
\ the require reads the file.
: CAPTURE-REPL-LINES ( -- )
   s" AOT-ARM:WINDOW-OPEN" DRV-LINE
   HB-TARGET-LINUX? if s" include src/os/linux/repl-term.f"
   else s" include src/os/macos/repl-term.f" then DRV-LINE
   s" include src/habu/repl.f" DRV-LINE
   s" require src/habu/debug.f" DRV-LINE ;

\ AOT-ARM owns every span and the checker payload. The boot-run list is restated
\ because CAPTURE resets it, with the three REPL entries first so the engine
\ installs its REPL before the fixture runs. HABU_AOT_WID_NARROW=N pokes the
\ window's wordlist end back by N after it was latched, so the window declares
\ fewer ids than it made; the fixture's package is the last thing the window
\ creates, so the short end reaches it. The capture must refuse at build time,
\ naming the record.
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
   S\" s\" DEBUG:BPW-INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE
   S\" s\" DEBUG:S-INSTALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ Two packages, one protected. The unprotected one comes first so the protected
\ one's window-relative offset is not zero: a seed that dropped the offset and
\ protected WIDN itself would pass a fixture whose only protected wordlist was
\ the window's first. Protection is asked for through `prot-wid-add`, the public
\ word any package uses, and the capture derives what travels from the live band
\ (aot-capture.f ACAP-PWIN-CAPTURE) as the real metabuild does. A captured
\ protected WID is stored as its offset from the window's first wordlist id
\ (AOT-BUF:AOT-WID-W0) and the seed rebases it onto the booting engine's WIDN
\ (habu2.f AOT-WINDOW:SEAL-WIDS,), so this file cannot name the protected id:
\ test/aot-wid-suite.f asks the built engine by package name
\ (tools/pkg-wid-probe.f).
\ The check takes two captures over one window: the first, with nothing
\ protected, must be accepted and record no row; the second, after
\ `prot-wid-add`, must record exactly the live protected WIDs inside the window.
\ The host's own 100-odd protected wordlists sit below the window and are
\ dropped, since replaying them would resurrect the discarded namespace. Both
\ run in the metabuild host through the capture's own words, the only place the
\ contract can be checked against a live band.
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

\ HABU_AOT_BAKE=1: an initialised cell plus a word, run from the boot-run list,
\ that reads it. The reference is a DATA address literal inside the window, so
\ the built engine reports the value at boot only if the content travelled and
\ that literal was rebased onto the seeded DP.
: BAKE-FIXTURE-LINES ( -- )
   s" create AWB-CELL 8 allot" DRV-LINE
   s" $5A5AC0DEC0DE5A5A AWB-CELL !" DRV-LINE
   S\" : AWB-REPORT ( -- ) s\" awb-cell=\" type AWB-CELL @ . cr ;" DRV-LINE
   RECAPTURE-LINE
   REPL-BOOTRUN-LINES
   S\" s\" AWB-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ HABU_AOT_TRAP=1: a deferred word nothing installs, called from the boot-run
\ list. Its dispatch cell is a declared address cell inside the window, so the
\ capture zeroed it and the seed wrote the trap xt back: calling it must reach
\ DEFER-UNSET's named die, "defer: unset execution vector", not the baked bytes.
: TRAP-FIXTURE-LINES ( -- )
   s" defer AWB-VEC ( -- )" DRV-LINE
   s" : AWB-CALL ( -- ) AWB-VEC ;" DRV-LINE
   RECAPTURE-LINE
   REPL-BOOTRUN-LINES
   S\" s\" AWB-CALL\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

\ HABU_AOT_GATE=1. The seed resolves each baked name in the booting engine, then
\ rewrites a call immediate, writes an xt into a code literal or branches to the
\ word; LAOTWIDGATE stands between the lookup and all three. It admits a
\ wordlist this seed created, then the two engine-reserved ones, then any
\ unprotected one, and a sealed pre-existing wordlist only as a package's public
\ slot. Nothing else in the tree reaches that routine. An in-window package is
\ one the seed creates, which the first layer admits, so the wordlist under test
\ must be one the target already had: the window compiles, and never runs, a
\ word that calls a qualified prefix word. The capture stores that site with the
\ qualified marker, and at boot LFIND resolves it through the package row's
\ public slot, the wid the gate asks about; the gate decides at seed time,
\ before any window code runs. CHECKER-TAPE's package is unsealed
\ (src/core/checker.f seals neither wordlist; GATE-SEAL-CHECK-LINES asserts it)
\ and publishes a word whose body reads a pre-window data cell, so the
\ compile-mode inliner declines the copy and emits the BL this fixture needs; a
\ constant would be inlined and leave no call site.
: GATE-SEAL-CHECK-LINES ( -- )
   GATE-ENV$ nip 0= if exit then
   s" : AWB-PKG-PUB ( ptr u8 n -- n ) DICT-WL:NAMESPACE search-wl ;" DRV-LINE
   s" : AWB-PROT ( n -- n ) {: w:n :}" DRV-LINE
   s"    w 6 rshift 8 * data-base PROT-BITS-OFF + + @  w 63 and rshift  1 and ;" DRV-LINE
   s" : AWB-?UNSEALED ( ptr u8 n -- ) {: a:ptr u:n :}" DRV-LINE
   s"    a u AWB-PKG-PUB {: w:n :}" DRV-LINE
   s"    w 0= if" DRV-LINE
   S\"       s\" aot-wid-build: the gate fixture's package is gone: \" type a u type cr" DRV-LINE
   S\"       s\" aot-wid-build: gate fixture package missing\" 74 die then" DRV-LINE
   s"    w AWB-PROT 0= if exit then" DRV-LINE
   S\"    s\" aot-wid-build: gate fixture package \" type a u type" DRV-LINE
   S\"    s\" : sealed, but the mode needs an unsealed one\" type cr" DRV-LINE
   S\"    s\" aot-wid-build: gate fixture seal assumption broken\" 74 die ;" DRV-LINE
   S\" s\" CHECKER-TAPE\" AWB-?UNSEALED" DRV-LINE ;

: GATE-ENTRY-LINES ( -- )
   s" package AWBGATE" DRV-LINE
   s" public" DRV-LINE
   s" : AWB-GATE-CALL ( -- ) CHECKER-TAPE:DISARM ;" DRV-LINE
   S\" : AWB-GATE-REPORT ( -- ) s\" awb-gate=open\" type cr ;" DRV-LINE ;

: GATE-FIXTURE-LINES ( -- )
   s" variable AWB-GATE-WID" DRV-LINE
   GATE-ENTRY-LINES
   s" get-current AWB-GATE-WID !" DRV-LINE
   s" ;package" DRV-LINE
   \ The target collision fixture allocates its own owner at this actual host
   \ ordinal before seeding. Neither process borrows the other's WIDN.
   S\" s\" awb-source-wid=\" type AWB-GATE-WID @ . cr" DRV-LINE
   RECAPTURE-LINE
   REPL-BOOTRUN-LINES
   S\" s\" AWBGATE:AWB-GATE-REPORT\" AOT-CAPTURE:BOOTRUN+" DRV-LINE ;

: FIXTURE-LINES ( -- )
   GATE-ENV$ nip 0 > if GATE-FIXTURE-LINES exit then
   BAKE-ENV$ nip 0 > if BAKE-FIXTURE-LINES exit then
   TRAP-ENV$ nip 0 > if TRAP-FIXTURE-LINES exit then
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
   s" ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R variable PRE-D PRE-D ! PRE-R !" DRV-LINE
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
   S\"    s\" src/habu/code-bytes.f\" AOT-IDENT:PATH+" DRV-LINE
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
   DRV-PATH$ DRV-BUF DRV-U @ FS:WRITE-ALL ;

: FORGE? ( -- bool )
   SPAN-ENV$ nip 0 >
   s" HABU_AOT_WID_SKEW" GETENV nip 0 > or
   s" HABU_AOT_WID_SPAN" GETENV nip 0 > or ;

\ The program the writer image runs on stdin once its own write is done: empty
\ unless a forge is asked for, which patches the emitted scalar in the writer's
\ CODE and then rebuilds and re-signs the image in that same process.
: PATCH-PROGRAM ( -- )
   DRV-RESET
   FORGE? 0= if exit then
   s" package AOT-WID-IMAGE-PATCH" DRV-LINE
   s" : PATCH-CELL ( n n label -- ) {: value:n old:n lab:label :}" DRV-LINE
   s"    lab A64ICODE:LBL-BOUND? 0= if 79 throw then" DRV-LINE
   s"    lab LABEL>N A64ICODE:LBLP @ 4 * {: off:n :}" DRV-LINE
   s"    off 0< off CODELEN @ 8 - > or if 79 throw then" DRV-LINE
   s"    A64ICODE:CODE off + {: dst:ptr :}" DRV-LINE
   s"    0 8 0 ?do dst i + c@ i 8 * lshift or loop old <> if 79 throw then" DRV-LINE
   s"    8 0 ?do value i 8 * rshift dst i + c! loop ;" DRV-LINE
   s" : RUN ( -- )" DRV-LINE
   SPAN-FORGE-LINE WID-FORGE-LINE
   s"    SIGN-ID:ENGINE$ 0 SCRIPT-ARGV$ DRV-EMIT-IMAGE" DRV-LINE
   s"    0 SCRIPT-ARGV$ CODESIGN:ENSURE ;" DRV-LINE
   s" RUN" DRV-LINE
   s" ;package" DRV-LINE ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: CHILD ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n input:ptr inputu:n :}
   path pathu >LEN input inputu >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   \ An expired deadline leaves with PROC-TIMEOUT-RC, which the rows that run
   \ this builder throw again as E-PROC-TIMEOUT.
   rc PROC-TIMEOUT-RC = if s" aot-wid-build: child ran out of time" rc die then
   rc 0<> if s" aot-wid-build: child failed" rc die then ;

\ The cold host depends on nothing this builder varies, so every mode - and
\ every other fixture - takes the one test/cold-engine.f shares through the
\ selected cache, emitting it only on a miss.
: BUILD-COLD ( -- )
   COLD$ COLD-ENGINE:PROVIDE ;

: CAPTURE-FIXTURE ( -- )
   ARGS s" --load" ARG DRV-PATH$ ARG s" --" ARG ART$ ARG COLD$ ARG
   COLD$ NULL$ CHILD ;

\ The artifact crosses to a fresh optimizing native writer process, whose cold
\ product reads the restoring checkout's prefix before seeding, so a private
\ top-row copy can establish a real target WID collision. The writer path comes
\ first: see FIXTURE-WRITER:PATH$.
: WRITE-FIXTURE ( -- )
   FIXTURE-WRITER:PATH$ {: writer:ptr writeru:n :}
   PATCH-PROGRAM
   ARGS s" --" ARG IMAGE$ ARG ART$ ARG COLD$ ARG
   writer writeru DRV-BUF DRV-U @ CHILD ;

: REWRITE? ( -- bool )
   s" HABU_AOT_REWRITE" GETENV nip 0 > ;

\ HABU_AOT_REWRITE=1 takes no capture: it reruns the writer over the window.aot
\ and hb-cold an earlier build left in TMPDIR, replacing its hb-pwid with one
\ carrying the forge asked for. A forge changes only the writer's patch, never
\ the window, so a suite forging one window several ways captures it once. The
\ writer checks the artifact against its host; without both there is nothing to
\ rewrite, and building afresh would hide that. The image it replaces goes
\ first, so an image after the rewrite can only be the rewrite's.
: REWRITE-SETUP ( -- )
   ART$ FS:EXISTS? COLD$ FS:EXECUTABLE? and 0= if
      s" aot-wid-build: rewrite found no earlier capture in TMPDIR" BUILD-RC die then
   IMAGE$ FS:EXISTS? if IMAGE$ FS:REMOVE-FILE then ;

public

: BUILD ( -- )
   PATHS
   REWRITE? if
      REWRITE-SETUP
   else
      GEN-DRIVER
      BUILD-COLD
      CAPTURE-FIXTURE
   then
   WRITE-FIXTURE
   s" aot-wid-build: hb-pwid ready" type cr ;

;package

AOT-WID-BUILD:BUILD

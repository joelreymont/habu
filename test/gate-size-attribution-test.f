\ gate-size-attribution-test.f - committed byte-attribution manifest + gate.
\
\ Companion to test/gate-build-size.f: that gate ratchets the whole-file size;
\ this one commits how those bytes decompose into named regions and fails when the
\ decomposition stops reconciling. The live emitter (src/habu/driver-io.f
\ DRV-SIZE-MAP) attributes every byte at build time and fails closed on any
\ residue, tools/size-report.f renders and reconciles a captured map, and this file
\ pins the committed per-region totals plus the distance-to-page-floor per target.
\
\ macOS is measured at the byte-fixpoint (2026-07-19, shared PROT-GUARD:CALL
\ span-guard fold); Linux at the byte-fixpoint (2026-07-19, DGX Spark linux-arm64).
\ Both targets' whole-file totals are coupled to the live engine and both
\ per-region splits are committed below.

require lib/test.f
require tools/size-report.f

package SIZE-ATTR

\ Image-writer region constants (src/os/macos/{layout,macho,sign2}.f,
\ src/os/linux/{layout,elf}.f).
$1000 constant CODE-OFF          \ header + load commands padded to the code offset
$4000 constant MACOS-PAGE        \ __TEXT segment page (16 KiB)
$1000 constant LINUX-PAGE        \ text segment page (4 KiB)
$4000 constant MACOS-DATA-CONST  \ __DATA_CONST page (__got + zero fill)
104 constant MACOS-LINKEDIT      \ __LINKEDIT chained fixups (MACHO-FIXUPS-SIZE)

\ macOS committed attribution, re-measured live at the byte-fixpoint on
\ 2026-07-19 for the MATCH dispatch B.cond slimming (dot
\ habu-slim-match-emitted-66941fb5). Reductions 1-3 shrink the EMITTED per-arm
\ match dispatch (movz+ldur+cmp+cset+cbz per arm -> one shared ldur + cmp #tag +
\ b.ne), but the ENGINE emitter grows by three small additions: the imm12 range
\ check + immediate-cmp path in EM-ADT-MATCH-OF, the class-based imm26/imm19
\ selector in LPAT, and J-MATCH's single tag-peek emit. That change alone is +28
\ bytes of engine text (matching spark's independent linux-arm64 measurement,
\ 136540 -> 136568). Re-measuring the live macOS byte-fixpoint on this tree gives
\ CODELEN 127860 (floor 884). The previous row (127420, floor 444) had drifted
\ BELOW the true feae4380 fixpoint (127832): page-absorbed engine growth from the
\ merges since the row was last set, invisible to the whole-file GB-SIZE ratchet
\ (exactly the drift the exact-CODELEN ratchet exists to close). So this row jumps
\ 127420 -> 127860 = +412 stale-drift correction + my +28. Header+code still fits
\ nine 16 KiB __TEXT pages, so the code signature and whole-file total are
\ unchanged. Keep MACOS-TOTAL equal to GB-SIZE-BASELINE-MACOS in
\ test/gate-build-size.f.
\ Lowered again on 2026-07-19 after the DP out-of-range die rework landed from
\ the Linux side (commit cffe4d44): that engine change shrinks macOS __text by
\ 96 bytes, the Linux rows were re-measured by its author, and the exact-CODELEN
\ ratchet (STALE-BASELINE, candidate 127764 vs row 127860) forces this host's
\ honest lowering: CODELEN 127860 -> 127764, floor 884 -> 788. Page count,
\ signature, and whole-file total are unchanged.
\ 2026-07-19 re-measured live at the merged macOS fixpoint: the MATCH stencil
\ factoring (habu-factor-repeated-match, measured -408 on linux-arm64 by its
\ author) shaves macOS compiler text too, and the new getpid process-identity
\ primitive adds its emitter + registration; the exact-CODELEN ratchet measured
\ the composed candidate at 127536 (STALE-BASELINE, was 127764), floor 788 ->
\ 560. Page count, signature, and whole-file total are unchanged.
\ 2026-07-19 re-measured live at the macOS byte-fixpoint for the CASE OF dispatch
\ B.cond slimming (dot habu-branch-directly-on-4624d193). J-OF now emits
\ cmp; b.ne next; drop instead of cmp; cset x9,eq; cbz x9,next; drop, so the
\ engine's J-OF drops exactly one C-EMITW call site (the cset word), shrinking
\ macOS __text by 12 bytes; the class-based imm26/imm19 LPAT selector needed to
\ patch the b.ne placeholder already landed with the MATCH slimming. The
\ exact-CODELEN ratchet measured the candidate at 127524 (STALE-BASELINE, was
\ 127536), floor 560 -> 548. Page count, signature, and whole-file total are
\ unchanged. (Controlled fixtures: one-arm CASE 124 -> 120, two-arm 180 -> 172,
\ exactly 4 bytes and one executed instruction per arm.)
\ 2026-07-20 re-measured live at the macOS byte-fixpoint for the checked PTY
\ lifecycle campaign's process primitives: proc-watch-open (kqueue +
\ EVFILT_PROC/NOTE_EXIT emitter with its carry-checked failure path) plus
\ kill-errno and execve (carry-flag errno negation each), with their FPRIM
\ registrations and checker axioms. The exact-CODELEN ratchet measured the
\ composed candidate at 127960 (STALE-BASELINE, was 127524), floor 548 -> 984.
\ Page count, signature, and whole-file total are unchanged (the __TEXT pad
\ absorbs the growth).
\ 2026-07-20 re-measured live at the composed macOS fixpoint after rebasing onto
\ spark's linux-side landings: the AOT DATA-reserve boot guard + CAP-PEND ring
\ (src/habu/habu2.f, spark measured +104 on linux-arm64) grow macOS engine text
\ by +88, and the JIT constant-push / binary-prep de-duplication
\ (dot habu-share-duplicated-jit, spark measured -272 on linux-arm64) shaves the
\ exact same -272 on macOS. The exact-CODELEN ratchet measured the composed
\ candidate at 127776 (STALE-BASELINE, was 127960), floor 984 -> 800. Page
\ count, signature, and whole-file total are unchanged (the __TEXT pad absorbs
\ the delta).
\ 2026-07-20 re-measured live at the macOS-aarch64 fixpoint after the `using NAME
\ … ;using` consumer-import landed (dot habu-using-import-pkg-a07dd7ba): the
\ used-publics resolver leaf (EMIT-FIND-USED), the `using`/`;using` keyword
\ emitters + their diagnostics, the interpret/compile/tick used-search injections,
\ and the eval-frame / REPL / package using-depth snapshots add +1952 of engine
\ text, CODELEN 127776 -> 129728 (floor 800 -> 2752); the whole file stays inside
\ the same 16 KiB page (SIGNATURE and MACOS-TOTAL unchanged, the __TEXT pad absorbs
\ the delta).
\ 2026-07-20 re-measured live at the macOS-aarch64 byte fixpoint after the JIT
\ region moved within BL range of __text (dot habu-map-the-code-5268af94, landed
\ 80636de2): that commit grew engine text on both targets but only bumped the Linux
\ row (its author measured linux-arm64), leaving this macOS row stale. The same
\ hint-and-verify mmap + boot BL-range assertion, snapshot-restore/BSNAPREBASE
\ region-sentinel passes, saved-DBASE crash test, and LBLRANGE diagnostic add +160
\ of engine text here, CODELEN 129728 -> 129888 (floor 2752 -> 2912); the whole file
\ stays inside the same 16 KiB page (SIGNATURE and MACOS-TOTAL unchanged, the __TEXT
\ pad absorbs the delta). The deterministic-AOT-region-baking fix in this commit is
\ host build-time only (src/habu/aot-capture.f is not baked into bin/hb), so it does
\ not move CODELEN; this row reflects only the region-move's owed macOS re-measure.
\ 2026-07-21 re-measured live at the macOS-aarch64 byte fixpoint (bin/hb rebuilt to
\ the byte-for-byte install --force fixpoint, then HABU_ENGINE_SIZE_MAP=1 captured and
\ reconciled with zero residue). Three shared-engine landings that spark re-measured
\ on linux-arm64 only - it cannot build or measure macOS - had left this macOS row
\ stale: the direct-BL call emitter (dot habu-aot-repl-bl, commit bc19e56e; it folds
\ the absolute movz/movk/movk x16 + blr x16 call sequence into the shared LCEMITBL
\ primitive and shrinks the captured AOT-REPL blob, linux -3344), the checked munmap
\ primitive (dot habu-expose-checked-mmap-06c1d522, commit 541b691f; native BMUNMAP
\ body, linux +136), and the shared declaration-event transaction (src/core/decl-event.f,
\ dot habu-type-declarations-shared-14ab0e48, commit 8763905f; habu2.f wiring, linux
\ +44). All three are src/habu baked-engine changes, and their macOS __text delta sums
\ byte-for-byte to the linux measurement: CODELEN 129888 -> 126724 (-3164 = -3344 +136
\ +44). The interleaved rigid host-allocation identity domains (commit ac5901a3) and the
\ data-loader LOAD -> LOAD-CORPUS rename (commit 2afcc679) touch only runtime
\ checker/render/maki source loaded at boot, not baked __text, so they contribute 0
\ bytes here. The -3164 __text shrink crosses the 16 KiB __TEXT page floor: the text
\ segment falls from nine to eight 16 KiB pages (floor-dist 2912 -> 16132) and the
\ ad-hoc code signature loses four 4 KiB code-directory hash slots (1423 -> 1295 =
\ -128). Whole file 165367 -> 148855 (-16384 page - 128 signature = -16512).
\ MACOS-DATA-CONST and MACOS-LINKEDIT are unchanged. Keep MACOS-TOTAL equal to
\ GB-SIZE-BASELINE-MACOS in test/gate-build-size.f.
\ 2026-07-21 second owed re-measure: the FINDPTR primitive retirement (linux
\ commit 017524d8, spark cannot measure macOS) shrank baked __text by 392 bytes
\ within the same 16 KiB page, so only CODE-TEXT and the floor distance move;
\ signature and whole-file total are unchanged. Candidate ratchet measured 126332.
126332 constant MACOS-CODE-TEXT   \ CODELEN: every emitter-phase row (baked-source incl.)
1295 constant MACOS-SIGNATURE     \ ad-hoc code signature SuperBlob (grows with CODELEN)
15740 constant MACOS-FLOOR-DIST     \ code above the 16 KiB floor: the page-recovery shave
148855 constant MACOS-TOTAL       \ = FILE-SIZE bin/hb = GB-SIZE-BASELINE-MACOS

\ Linux committed attribution, measured at the byte-fixpoint on 2026-07-19 (DGX
\ Spark, linux-arm64) after the shared PROT-GUARD:CALL span-guard fold (CODELEN
\ 142092 -> 136108), then advanced by the same +28-byte shared-engine-text delta
\ as macOS above (LP2VEXEC record, predicted 136108 -> 136136), then re-measured
\ live at the 2026-07-19 seal-guard fixpoint: the raw ffi-call nargs guard adds
\ 404 bytes, CODELEN 136136 -> 136540 (floor 968 -> 1372), confirming the +28
\ prediction; all 432 bytes since the fold fit inside the same 4 KiB page
\ padding (LINUX-TOTAL unchanged). Keep LINUX-TOTAL equal to
\ GB-SIZE-BASELINE-LINUX in test/gate-build-size.f.
\ 2026-07-19 re-measured live at the merged linux-arm64 fixpoint (spark): MATCH
\ dispatch B.cond slimming adds +28 of compiler text (136540 -> 136568, matching
\ the macOS-measured delta) and the DP out-of-range named-die fix shaves -256
\ (two 8-byte silent exit stubs per DP-CHECK site fold to one 4-byte B LDPBAD),
\ composing to 136312 (floor 1372 -> 1144); the whole file stays inside the same
\ 4 KiB page (LINUX-TOTAL unchanged).
\ 2026-07-19 re-measured live at the linux-arm64 fixpoint (spark): factoring the
\ repeated ADT-dispatch stencils (dot habu-factor-repeated-match) into three
\ BL/B-shared engine routines (LADTPUSHTOK/LMFRTOP/LADTDIE) shaves -408 of
\ compiler text, all inside compile/adt (2548 -> 2140), composing to 135904
\ (floor 1144 -> 736); the whole file stays inside the same 4 KiB page
\ (LINUX-TOTAL unchanged).
\ 2026-07-19 re-measured live at the merged linux-arm64 fixpoint (spark) after
\ the getpid process-identity primitive + checked PTY authority registry landed
\ (macOS row above was re-measured by that landing; Linux left for this next
\ fixpoint): +92 of engine text, composing to 135996 (floor 736 -> 828); the
\ whole file stays inside the same 4 KiB page (LINUX-TOTAL unchanged).
\ 2026-07-19 re-measured live at the linux-arm64 fixpoint (spark) after the CASE
\ OF dispatch B.cond slimming (dot habu-branch-directly-on-4624d193): exactly the
\ macOS-predicted -12 (one C-EMITW call site dropped from J-OF), CODELEN 135996
\ -> 135984 (floor 828 -> 816); the whole file stays inside the same 4 KiB page
\ (LINUX-TOTAL unchanged).
\ 2026-07-20 re-measured live at the merged linux-arm64 fixpoint (spark): the AOT
\ DATA-reserve span guard (dot habu-guard-aot-data-49de2ee6) adds +104 of engine
\ text in aot-seed (headroom check + inline boot die), CODELEN 135984 -> 136088
\ (floor 816 -> 920); the whole file stays inside the same 4 KiB page
\ (LINUX-TOTAL unchanged).
\ 2026-07-20 re-measured live at the linux-arm64 fixpoint (spark): the JIT
\ constant-push / binary-prep de-duplication (dot habu-share-duplicated-jit) folds
\ two structural copies in src/habu/jit.f into one body each. LVPUSHF's 84-byte
\ tag-only copy of the constant-push body (label-span pushf 88 -> 8) collapses to a
\ movz+b wrapper into the shared LVPUSHT body, and LVBINIPREP's full 232-byte copy
\ of the LVBINPREP base (label-span biniprep 360 -> 160) collapses to a probe that
\ tail-branches into the single LVBINPREP body; the shared push body's frame widen
\ (16 -> 32, to carry the tag across LVSPILL) adds back 8, netting -272 of engine
\ text, CODELEN 136088 -> 135816 (floor 920 -> 648); the whole file stays inside
\ the same 4 KiB page (LINUX-TOTAL unchanged).
\ 2026-07-20 re-measured live at the linux-arm64 fixpoint (spark) after the Mac's
\ packaging wave landed (kill-errno/execve primitives, checker/proc-control
\ changes, REQUIRE-MAX $100 -> $200; macOS rows re-measured on that side, Linux
\ owed this measurement): +296 of engine text, CODELEN 135816 -> 136112
\ (floor 648 -> 944); the whole file stays inside the same 4 KiB page
\ (LINUX-TOTAL unchanged).
\ 2026-07-20 re-measured live at the linux-arm64 fixpoint (spark) after the Mac's
\ `using NAME … ;using` consumer-import landed (dot habu-using-import-pkg-a07dd7ba;
\ macOS measured +1952 on that side, Linux owed this measurement): +2128 of engine
\ text, CODELEN 136112 -> 138240 (floor 944 -> 3072); the whole file stays inside
\ the same 4 KiB page (LINUX-TOTAL unchanged).
\ 2026-07-20 re-measured after the JIT region moved within BL range of __text (dot
\ habu-map-the-code-5268af94): the hint-and-verify mmap + boot BL-range assertion, the
\ snapshot-restore/BSNAPREBASE region-sentinel passes, the crash-handler saved-DBASE
\ region test, and the LBLRANGE diagnostic add +176 of engine text, CODELEN 138240 ->
\ 138416 (floor 3072 -> 3248); the whole file stays inside the same 4 KiB page
\ (LINUX-TOTAL unchanged).
\ 2026-07-20 re-measured after every statically known native call became one direct BL
\ (dot habu-aot-repl-bl): the absolute movz/movk/movk x16 + blr x16 call emitter collapses
\ to the shared LCEMITBL primitive and the captured AOT-REPL blob loses 12 bytes per call
\ site, shrinking engine text by 3344, CODELEN 138416 -> 135072 (floor 3248 -> 4000); the
\ whole file drops one 4 KiB page (LINUX-TOTAL 143552 -> 139456).
\ 2026-07-20 re-measured at the merged fixpoint after the checked munmap primitive
\ landed on top of the direct-BL engine (dot habu-expose-checked-mmap-06c1d522):
\ native BMUNMAP + Gforth-recovery BMUNMAP + the checker munmap effect row, consumed
\ by packaged MEM:RELEASE-BYTES, add +136 of engine text, CODELEN 135072 -> 135208.
\ The BL landing left only 96 bytes above the 4 KiB floor (floor-dist 4000), so the
\ +136 crosses the page boundary: the file regains the page the BL landing dropped
\ (floor-dist 4000 -> 40, LINUX-TOTAL 139456 -> 143552).
\ 2026-07-20 re-measured at the merged fixpoint after the shared declaration-event
\ transaction landed (src/core/decl-event.f, dot habu-type-declarations-shared-14ab0e48):
\ +44 of engine text on the direct-BL+munmap base, CODELEN 135208 -> 135252 (floor
\ 40 -> 84); the whole file stays inside the same 4 KiB page (LINUX-TOTAL unchanged).
\ 2026-07-21 re-measured at the linux-arm64 fixpoint (spark) after retiring the
\ vestigial EM-SNAPSHOT-REBASE-CALLS snapshot scan (dot
\ habu-retire-vestigial-snapshot-e4187b76): the direct-BL landing left it a no-op (no
\ absolute movz/movk call chain survives to rebase), so deleting the routine, its four
\ BL call sites, and the restore-path dead x16 setup shaves -392 of engine text, CODELEN
\ 135252 -> 134860 (floor 84 -> 3788); the shave crosses the 4 KiB floor so the whole
\ file drops one page (LINUX-TOTAL 143552 -> 139456).
134860 constant LINUX-CODE-TEXT   \ CODELEN: every emitter-phase row (baked-source incl.)
192 constant LINUX-RW             \ ELF read-write segment tail: DYNAMIC + GOT (ELF-RW-SZ)
3788 constant LINUX-FLOOR-DIST     \ code above the 4 KiB floor: the page-recovery shave
139456 constant LINUX-TOTAL       \ = FILE-SIZE bin/hb = GB-SIZE-BASELINE-LINUX

: PAGE-UP ( n n -- n ) {: v:n page:n :}
   v page 1- + page 1- invert and ;

\ text-pad = the zero fill from the end of (header + code) up to the __TEXT page.
: MACOS-TEXT-PAD ( -- n )
   CODE-OFF MACOS-CODE-TEXT + MACOS-PAGE PAGE-UP
   CODE-OFF - MACOS-CODE-TEXT - ;

\ Reconstruct the whole file from the committed macOS regions.
: MACOS-MODEL-SUM ( -- n )
   CODE-OFF MACOS-CODE-TEXT + MACOS-TEXT-PAD +
   MACOS-DATA-CONST + MACOS-LINKEDIT + MACOS-SIGNATURE + ;

: MACOS-MODEL-FLOOR ( -- n )
   CODE-OFF MACOS-CODE-TEXT + MACOS-PAGE mod ;

\ text-pad = the zero fill from the end of (header + code) up to the text page.
: LINUX-TEXT-PAD ( -- n )
   CODE-OFF LINUX-CODE-TEXT + LINUX-PAGE PAGE-UP
   CODE-OFF - LINUX-CODE-TEXT - ;

\ Reconstruct the whole file from the committed Linux regions.
: LINUX-MODEL-SUM ( -- n )
   CODE-OFF LINUX-CODE-TEXT + LINUX-TEXT-PAD +
   LINUX-RW + ;

: LINUX-MODEL-FLOOR ( -- n )
   CODE-OFF LINUX-CODE-TEXT + LINUX-PAGE mod ;

\ The committed total for whichever target is running (0 = unmeasured -> the live
\ coupling below fails closed against the nonzero engine file).
: HOST-TOTAL ( -- n )
   HB-TARGET-MACOS? if MACOS-TOTAL exit then
   HB-TARGET-LINUX? if LINUX-TOTAL exit then
   0 ;

: LIVE-ENGINE ( -- n )
   s" bin/hb" FILE-SIZE ;

public

\ Committed CODE-TEXT (__text) row for whichever target is running (0 = unmeasured).
\ The gate's exact-CODELEN ratchet holds the candidate's measured SUM-TEXT to this.
: HOST-CODE-TEXT ( -- n )
   HB-TARGET-MACOS? if MACOS-CODE-TEXT exit then
   HB-TARGET-LINUX? if LINUX-CODE-TEXT exit then
   0 ;

\ Pure self-check (no build): each target's committed decomposition reconstructs
\ its whole file and page-floor shave, and the running target's committed total
\ equals the live installed engine. Any drift - a bigger engine, a stale row -
\ fails one of these, so the manifest cannot silently fall behind reality.
: RUN ( -- )
   T-RESET
   MACOS-MODEL-SUM MACOS-TOTAL T=
   MACOS-MODEL-FLOOR MACOS-FLOOR-DIST T=
   LINUX-MODEL-SUM LINUX-TOTAL T=
   LINUX-MODEL-FLOOR LINUX-FLOOR-DIST T=
   HOST-TOTAL LIVE-ENGINE T=
   T-REPORT ;

\ Live drift check against a captured map + its engine (for a build-and-capture
\ gate or a maintainer refresh). Reconciles every byte, then holds the measured
\ per-target regions to the committed rows.
: VALIDATE ( ptr u8 n ptr u8 n -- ) {: ma:ptr mu:n ea:ptr eu:n :}
   T-RESET
   ma mu SIZE-REPORT:LOAD
   ea eu SIZE-REPORT:RECONCILE
   ea eu FILE-SIZE SIZE-REPORT:SUM-ALL T=
   HB-TARGET-MACOS? if
      SIZE-REPORT:HEADER-BYTES CODE-OFF T=
      SIZE-REPORT:SUM-TEXT MACOS-CODE-TEXT T=
      SIZE-REPORT:FLOOR-DIST MACOS-FLOOR-DIST T=
      s" container/data-const" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH MACOS-DATA-CONST T=
      s" container/linkedit" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH MACOS-LINKEDIT T=
      s" container/signature" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH MACOS-SIGNATURE T=
   then
   HB-TARGET-LINUX? if
      SIZE-REPORT:HEADER-BYTES CODE-OFF T=
      SIZE-REPORT:SUM-TEXT LINUX-CODE-TEXT T=
      SIZE-REPORT:FLOOR-DIST LINUX-FLOOR-DIST T=
      s" container/rw-segment" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH LINUX-RW T=
   then
   T-REPORT ;

RUN

;package

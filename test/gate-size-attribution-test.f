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
\ merges since the row was last set, invisible to the whole-file BUILD-SIZE ratchet
\ (exactly the drift the exact-CODELEN ratchet exists to close). So this row jumps
\ 127420 -> 127860 = +412 stale-drift correction + my +28. Header+code still fits
\ nine 16 KiB __TEXT pages, so the code signature and whole-file total are
\ unchanged. Keep MACOS-TOTAL equal to BUILD-SIZE:BASELINE-MACOS in
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
\ BUILD-SIZE:BASELINE-MACOS in test/gate-build-size.f.
\ 2026-07-21 second owed re-measure: the FINDPTR primitive retirement (linux
\ commit 017524d8, spark cannot measure macOS) shrank baked __text by 392 bytes
\ within the same 16 KiB page, so only CODE-TEXT and the floor distance move;
\ signature and whole-file total are unchanged. Candidate ratchet measured 126332.
\ 2026-07-21 STRUCTURE front end landed (src/core/structure-decl.f, dot
\ habu-structure-parse-typed-c5a01e1f): the parser source is re-loaded from disk
\ in the boot run-prelude, not baked __text, so it contributes 0 bytes here; the
\ only baked-engine delta is its habu2.f boot wiring (the LPSTRUCTDECL label
\ variable + the load/path/provide rows + the label assignment), +48 macOS
\ __text measured on the lane base; composed on the post-FINDPTR baseline:
\ CODELEN 126332 -> 126424 (the +48 STRUCTURE wiring plus +44 from the concurrently landed linux-side AOT gate-entry work, both owed macOS re-measures), floor 15740 -> 15832. Whole file, signature, and
\ page count unchanged. The linux row below is owed a linux-host re-measure.
\ 2026-07-21 STRUCTURE constructor generator baked (src/core/structure-make.f, dot
\ habu-structure-generate-make-872a6e75): the reconciliation wires the front end's
\ ;STRUCTURE to STRUCTURE-MAKE:GENERATE, so structure-make.f now loads baked in the
\ post-hook DECL group (before structure-decl.f). Like the front end, the generator
\ source is re-loaded from disk in the boot run-prelude, not baked __text, so it
\ contributes 0 bytes here; the only baked-engine delta is its habu2.f boot wiring
\ (the LPSTRUCTMAKE label variable + the load/path/provide rows + the label
\ assignment), +48 macOS __text, measured live at the lane fixpoint: CODELEN 126424
\ -> 126472, floor 15832 -> 15880. Whole file, signature, and page count unchanged.
\ The linux row below is owed a linux-host re-measure.
\ 2026-07-21 owed re-measure at the fieldproj merge: spark's literal-split/per-site
\ relocation work (landed linux-side, cannot measure macOS) shrank baked __text by
\ 1336 bytes; candidate ratchet measured 125136. Same page, signature/total unchanged.
\ 2026-07-21 owed re-measure at the fieldproj merge (second composition): spark's
\ definer-publication landing shrank baked __text by a further 10692 bytes;
\ candidate ratchet measured 114444. Same page span per the floor arithmetic;
\ signature/total unchanged.
\ 2026-07-21 ENUM front end landed (src/core/enum-decl.f): parser source is
\ prelude-loaded from disk; the +40 baked delta is its habu2.f label wiring.
\ Candidate ratchet measured 114484.
\ 2026-07-28 loop-family opener guard landed (src/habu/habu2.f, dot
\ habu-fix-loop-closer-9e5d012e), composed with the macOS re-measure this row was
\ owed. Two separate amounts make up the +492, and they are not the same lane's
\ work. 444 of it arrived before this change: the lanes that landed between the
\ ENUM front-end row above and this one were measured on Linux and left the macOS
\ row stale, which the reconstruction proves - the committed MACOS-TOTAL 148855
\ already counted those bytes, so the old 114484 row modelled the file 444 short.
\ The remaining 48 belong to this change: the `J-LVREQUIRE` guard emitted at the
\ three loop-family compile sites, all of it inside `compile/keywords`, which the
\ live map shows moving 9680 -> 9728. Nothing else moves. The __TEXT pad absorbs
\ the whole 492 (container/text-pad 12048 -> 12000), so the file stays inside the
\ same 16 KiB page and MACOS-SIGNATURE, MACOS-DATA-CONST, MACOS-LINKEDIT and
\ MACOS-TOTAL are unchanged; with the new row the model sum reconstructs 148855
\ exactly. Candidate ratchet measured 114976, floor 3892 -> 4384. The linux row
\ below is owed a linux-host re-measure for the 48-byte guard.
\ 2026-07-28 raw-storage load-path seal landed (dot habu-seal-raw-storage): the
\ engine now registers every created word's effect through `trust-raw`, which
\ adds the C-FIND-TRUST-RAW resolver, its `trust-raw` keyword string, and the
\ three rewritten publication call sites to baked __text. On its own base it
\ measured +456 over the 114484 row - that is the 444-byte stale-row
\ correction the loop entry above documents plus 12 bytes of its own. Merged
\ here with the loop-family opener guard (+48), the fixpoint measures 114988
\ (floor 4384 -> 4396); the __TEXT pad absorbs the 12. Whole file,
\ signature, and page count unchanged; the linux row below is owed a
\ linux-host re-measure for both deltas.
\ 2026-07-31 re-measured live at the macOS-aarch64 byte fixpoint (install --force
\ run twice to a byte-identical bin/hb, then HABU_ENGINE_SIZE_MAP=1 captured off
\ the metabuild host and reconciled with zero residue). The snapshot-relocation
\ stack grew baked __text by 1680 bytes: CODELEN 114988 -> 116668, floor
\ 4396 -> 6076. The base of that growth is the tree that set the 114988 row
\ (`Seal raw storage nominals on the load path`), which re-measures to exactly
\ 114988 here, so no stale drift from an unmeasured lane is folded into this bump.
\ Four commits carry all of it, each measured at its own byte fixpoint:
\   +8   Validating snapshot names against RBASE-VA. The two dictionary-name
\        bounds checks in the owner-record scan now load the canonical RBASE-VA
\        into x13 instead of comparing against the live DBASE, which costs one
\        extra LIT64 per check. All 8 bytes are main/startup (5492 -> 5500).
\   +856 Declaring persisted region-address cells. The address-cell table and its
\        emitters (MARK-CELL, EMIT-CALLS, EMIT-MARK, EMIT-XT), the declaration
\        calls added at the `defer` cell site and the `is` store site, the
\        call-site bitmap the AOT patch loop now records into, and the
\        mmap-code rework that accepts whatever base the kernel returns instead
\        of demanding the hint. Regions: dictionary-code +472, main/startup +92,
\        primitives/cemitbl +72, runtime +72, compile/exit +68,
\        interpret/string +20, primitives/extra +20, interpret/define +16,
\        compile/keywords +12, primitives/base +12.
\   +708 Relocating persisted region address literals. MARK-SITE, EMIT-ADDR-SITE
\        and EMIT-ADDRS, plus the rewritten C-CODE-ADDR, C-DATA-ADDR and
\        C-DATA-ADDR-RAW that record every emitted region-address literal so the
\        loader can move it. Regions: dictionary-code +520, main/startup +64,
\        compile/exit +56, runtime +40, primitives/extra +16,
\        compile/keywords +12.
\   +108 Declaring persisted callback table cells. The `xt!` primitive
\        (SNAP-RELOC:BXTSTORE) stores an execution token and declares its cell in
\        one step, for the seven declaration-transaction store words whose cell
\        address is only worked out at run time. Regions: primitives/extra +60,
\        seed-dictionary +48.
\ The four amounts sum to exactly 1680 and every byte lands in a region that
\ already existed - none appeared and none vanished: main/startup +164,
\ interpret/define +16, interpret/string +20, compile/keywords +24,
\ compile/exit +124, primitives/base +12, primitives/extra +96,
\ primitives/cemitbl +72, dictionary-code +992, runtime +112, seed-dictionary +48.
\ The 16 KiB __TEXT page is NOT crossed: header + code is 120764 bytes, still
\ inside the same eight 16 KiB pages, and the text pad absorbs the whole 1680
\ (11988 -> 10308). MACOS-SIGNATURE, MACOS-DATA-CONST, MACOS-LINKEDIT and
\ MACOS-TOTAL are therefore unchanged, the model sum reconstructs 148855 exactly,
\ and MACOS-TOTAL still equals BUILD-SIZE:BASELINE-MACOS in test/gate-build-size.f.
\ The other ten commits on this stack that touch assembled engine source were each
\ measured too and move zero baked __text: the encode-time ARM64 operand guards,
\ the checker's source-tape observer seam, the persisted producer-xt cells, the
\ absent-package-context reject, and the two master merges all measure the same
\ CODELEN as the commit before them. Definitions added to boot-time source the
\ engine re-reads at launch cost nothing here, which is why the census grew by 33
\ while only four of those changes moved a byte of __text. The native compiler
\ chain under src/compiler/ is not assembled engine source and contributes
\ nothing. The linux row below is owed a linux-host re-measure for all 1680.
\ 2026-08-01 re-measured live at the macOS-aarch64 byte fixpoint after the
\ protected-WID registry became a WID-indexed bitmap (dot
\ habu-replace-the-protected-ca920a8f). Procedure: install --force to a
\ byte-identical bin/hb (sha c81c2ca8), then HABU_ENGINE_SIZE_MAP=1 captured on
\ the same tree and reconciled through tools/size-report.f with zero residue
\ (attributed 148855 = engine-file 148855). The base of the change was measured
\ the same way and reproduces this row's previous value 116668 exactly, so no
\ unmeasured lane's drift is folded into the bump. CODELEN 116668 -> 117740
\ (+1072), floor 6076 -> 7148. Five regions move and the five deltas sum to
\ exactly 1072; no region appeared and none vanished:
\   +896 aot-seed (22608 -> 23504). Two opposite amounts. The baked
\        protected-WID frame grows by exactly 1024: it was an 8-byte count
\        followed by zero rows (the shipped engine protects nothing at build
\        time) and is now an 8-byte shape tag followed by the full
\        PROT-BITS-BYTES image, always emitted at full width so the owner frame
\        behind it sits at a constant offset. The stage self-rebuild
\        generations, whose seed is frame-only, show that 1024 undiluted
\        (aot-seed 124 -> 1148). Against it, the captured AOT-REPL blob shrinks
\        128: LIT64, emits one move-wide word per 16-bit chunk that is neither
\        0 nor all-ones (src/arch/arm64/icode.f), so the absolute-address
\        literals inside the captured blob re-encode in 32 fewer instructions
\        once the 1072-byte layout shift moves the addresses they carry.
\   +84  main/startup (5656 -> 5740). EM-STARTUP-RUNTIME-STATE's cold init:
\        "the registry starts empty" was one store of 0 into the count cell and
\        is now a full zeroing loop over the band plus the release-publish of
\        the shape tag. Same region, the AOT restore and the snapshot-image
\        validator: EM-AOT-REGISTER-PROT-WIDS copies a fixed-width blob and
\        finds the highest set bit instead of walking rows, and the snapshot
\        validator's row scan with its nested duplicate check becomes a tag
\        compare plus a bit test.
\   +52  primitives/protected-wid (1524 -> 1576). EMIT-PROTWID loses its scan
\        loop and gains the bound check, the address computation and the
\        acquire-load — fewer executed instructions per membership test, very
\        slightly more emitted text.
\   +24  primitives/aot-owner (1384 -> 1408). The owner-restore's "is this WID
\        already protected" test: two bit tests where two full table scans stood.
\   +16  primitives/base (13740 -> 13756). The prot-wid-add and prot-wid-room
\        bodies: the former trades the count load and row store for the range
\        check and the read-modify-write of one word, the latter reports room
\        against WIDN rather than the count cell.
\ The 16 KiB __TEXT page is NOT crossed: header + code is 121836 bytes, still
\ inside the same eight 16 KiB pages, and the text pad absorbs the whole 1072
\ (10308 -> 9236). MACOS-SIGNATURE, MACOS-DATA-CONST, MACOS-LINKEDIT and
\ MACOS-TOTAL are unchanged, the model sum reconstructs 148855 exactly, and
\ MACOS-TOTAL still equals BUILD-SIZE:BASELINE-MACOS in test/gate-build-size.f.
\ The registry-exhaustion diagnostic that rides the same rebuild (dot
\ habu-name-registry-exhaustion-bdd23c70) was measured separately on top of the
\ bitmap tree and moves zero bytes here: its REASON-PROTECTION row lives in
\ src/core/generated-declaration.f, boot-time source the engine re-reads at
\ launch rather than baked __text. The linux row below is owed a linux-host
\ re-measure for all 1072.
\ 2026-08-05 MERGE macOS lowering 117740 -> 114236, measured live at the merged
\ proofs/master fixpoint (bin/hb sha d34e5ee8..., byte-identical over two
\ `install --force` runs). The merged engine is SMALLER than either parent's row:
\ proofs committed 117740 and master committed 114484, and the union carries
\ master's owner-registry deletion (the owner-WID emitters, the owner AOT
\ routines, the owner half of the snapshot WID validation) on top of proofs'
\ engine text. The exact-CODELEN ratchet reported STALE-BASELINE candidate 114236
\ against the proofs row, which is the shrink this lowering commits. Floor
\ follows from the same number - MACOS-MODEL-FLOOR is (CODE-OFF + CODE-TEXT) mod
\ 16 KiB - so 7148 -> 3644, exactly master's 3892 less the further 248 bytes.
\ MACOS-SIGNATURE and MACOS-TOTAL do not move: the shave stays inside the same
\ eight 16 KiB __TEXT pages, so the text pad absorbs all 3504 bytes and
\ `FILE-SIZE bin/hb` is still 148855 = BUILD-SIZE:BASELINE-MACOS. The Linux row
\ below is owed a linux-host re-measure for the same merge.
\ 2026-08-05, the bulk publication window (dot habu-publish-native-code-886e3ef9):
\ three engine primitives join the emitter - `code-publish` (the span guard, the
\ RW/copy/RX bracket, the call-map clear over the span and the range flush),
\ `callmap-set` (one relocation bit) and `xref-retarget` (both record cells in
\ one window, length then start-release) - plus their registrations and checker
\ axioms. They replace a per-instruction poke loop in the CALLER
\ (src/compiler/native/publish.f), so the engine text grows while the work each
\ publication does shrinks. Two of them also carry their own bounds guard - a
\ call-site address outside the region would index past the call map, and a
\ record index outside the live dictionary would write anywhere in it - which is
\ the last 56 bytes. The exact-CODELEN ratchet measured the candidate at 115028
\ (was 114236), +792. Floor follows from the same number: 3644 -> 4436, inside
\ the same 16 KiB page, so the text pad absorbs it and neither MACOS-SIGNATURE
\ nor MACOS-TOTAL moves.
\ 2026-08-05, the dictionary-index lifetime window (dots
\ habu-keep-the-dictionary-7fb71873 / habu-idx-imported-name-ab38fcac): the
\ hash-index insert now counts claimed slots and LHIDXADD compacts the table in
\ place at the load bound instead of ever silently zeroing HIDXP (the new
\ HIDX:LREBUILD body, the claims counting, and the HIDX:LFULL loud exit), the
\ raise leg of ndict! calls the same compaction, and the used-public and
\ using-package lookups gained the per-wid hash probes in front of their
\ retained linear scans. The exact-CODELEN ratchet measured the candidate at
\ 116052 (was 115028), +1024. Floor follows from the same number: 4436 -> 5460,
\ inside the same 16 KiB page, so the text pad absorbs it and neither
\ MACOS-SIGNATURE nor MACOS-TOTAL moves (FILE-SIZE bin/hb still 148855).
\ 2026-08-06, the address-cell band window (dot
\ habu-seal-the-declaration-7183177e): the engine now refuses a declared
\ persisted-address cell that does not lie wholly inside DATA. EMIT-MARK gains
\ the bound compare and a named refusal tail, BXTSTORE reorders so the cell is
\ declared before it is stored, and the loader's EMIT-XT re-validates every row
\ it reads out of an image file and carries its own refusal tail. The exact-
\ CODELEN ratchet measured the candidate at 116204 (was 116052), +152. Floor
\ follows from the same number: 5460 -> 5612, inside the same 16 KiB page, so the
\ text pad absorbs it and neither MACOS-SIGNATURE nor MACOS-TOTAL moves
\ (FILE-SIZE bin/hb still 148855, signature still 1295).
\ 2026-08-06, the held publish exit (dot habu-give-the-chain-5ed1f7c5): the
\ compile-mode publish tail gains a third outcome. A definition the native chain
\ is recording may be CERTIFIED and withheld - the count, the name index and the
\ record facts are all skipped and the code pointer returns to the colon entry -
\ so the chain's own publisher can commit the record once it has an emission the
\ validator accepted. The question is asked where the two publish tails converge,
\ by the same named-checker-word lookup EM-REC-WIDE-PUBLISH already uses a few
\ instructions later, because only one of the two tails carries a hook verdict at
\ all. That is a lookup, a call, a test and a four-instruction exit block. The
\ exact-CODELEN ratchet measured the candidate at 116332 (was 116204), +128.
\ Floor follows from the same number: 5612 -> 5740, inside the same 16 KiB page,
\ so the text pad absorbs it and neither MACOS-SIGNATURE nor MACOS-TOTAL moves.
\ 2026-08-10, the address chain's own register (dot habu-widen-emit-addrs, folded
\ into habu-per-site-relocation): SNAP-RELOC:EMIT-ADDRS stops demanding x9 in all
\ four lanes of a recorded chain and instead takes the register from the site's
\ own first word, requiring the other three to name that same one. Thirteen
\ instructions, +52 = 13 * 4 (measured 116332 -> 116384 on its own base; composed
\ below with the loader rows by prediction, confirmed by this test's own measure).
\ 2026-08-10, the loader consults its registry (dot
\ habu-make-load-consult-85c88fb3) and states two facts it used to leave to
\ proxies. `bin/hb --load` stops inlining every argv file's text and appends
\ `s" <path>" script-required` instead, which costs a second append leaf beside
\ LAPPPROV (LAPPREQ: the same STRB-and-branch sequence with a longer keyword),
\ the three-way mode store in C-SOURCE-FILE-PREFIX and the branch in
\ C-SOURCE-APPEND-ARG that picks between the two. A second cold-prefix token,
\ REQUIRE-BOOT-FREEZE, is appended beside SEAL-CAPTURE so the count of paths the
\ ENGINE provides stays separable from the ones a process later required; it is
\ emitted the same inline-character way SEAL-CAPTURE is, with a longer keyword.
\ The exact-CODELEN ratchet measured the candidate at 117740 (was 116332),
\ +1408. Floor follows from the same number: 5740 -> 7148, and the text pad
\ absorbs it exactly - 9816 -> 9236, so code+pad is 126976 either way, the file
\ stays inside the same 16 KiB page and neither MACOS-SIGNATURE nor MACOS-TOTAL
\ moves (FILE-SIZE bin/hb still 148855, signature still 1295).
\ 2026-08-10, the boot prefix gains the checked stdlib (dot
\ habu-seed-the-stdlib-d8e3a757). The eight files are read from disk at boot and
\ contribute no emitter text of their own, but their TABLE ROWS do: eight
\ load rows and eight provide rows are an ADR and a BL each inside the
\ cold-prefix routine, the eight path rows bake their path bytes, and the
\ block's SNAP-CELL guard is three more instructions. Measured, not assumed -
\ the candidate was built at this commit's parent and at this commit and the
\ size maps differed: 117740 -> 118032, +292. Floor follows: 7148 -> 7440, pad
\ 9236 -> 8944, code+pad 126976 either way, so the file stays inside the same
\ 16 KiB page and neither MACOS-SIGNATURE nor MACOS-TOTAL moves.
   \ CODELEN: every emitter-phase row (baked-source incl.)
\ 2026-08-10, the address-literal map's publisher half (dot
\ habu-per-site-relocation-bb9b6d70): the engine gains `addrmap-set`, the
\ sibling of `callmap-set` for the SECOND relocation map - the one that records
\ which region word an address chain starts in, so a compiler written in Habu can
\ write the record the engine's own C-CODE-ADDR writes from inside. Same shape as
\ its sibling: the same region-offset bound guard with its named refusal tail,
\ the same two index computations, the same byte read-modify-write. The routine
\ disassembles to 32 instruction words against `callmap-set`'s 31 - one more
\ because ADDRMAP-OFF sits above CALLMAP-OFF and its LIT64 needs one further
\ move-wide lane - and the balance of the delta is the primitive's registration
\ and its deref entry. The exact-CODELEN ratchet measured the candidate at
\ 116584 (was 116384), +200. Floor follows from the same number: 5792 -> 5992,
\ inside the same 16 KiB page, so the text pad absorbs it and neither
\ MACOS-SIGNATURE nor MACOS-TOTAL moves.
\ 2026-08-11, the address map records DATA chains too (dot
\ habu-per-site-relocation-bb9b6d70): C-DATA-ADDR and C-DATA-ADDR-RAW now record
\ their site through SNAP-RELOC:MARK-SITE, the way C-CODE-ADDR always has, so the
\ AOT capture can FIND a chain-compiled DATA address instead of recognising one
\ by the value it carries. One BL per emit site and there are three of them -
\ EMIT-CREATE's pushed address and the two deferred-word dispatch-cell addresses.
\ The snapshot pass is unaffected: it is called once per band and a DATA address
\ is in neither of its bands, so a recorded DATA site is visited and rewritten by
\ neither. The exact-CODELEN ratchet measured the candidate at 118296 (was
\ 118284), +12 = 3 * 4. Floor follows from the same number: 7692 -> 7704, inside
\ the same 16 KiB page, so the text pad absorbs it and neither MACOS-SIGNATURE
\ nor MACOS-TOTAL moves.
\ 2026-08-11, the inliner carries a copied chain's record (dot
\ habu-per-site-relocation-bb9b6d70): C-CALL copies a short callee's body into
\ its caller instead of calling it, and an address chain in that body used to
\ arrive at its new region offset with no record, invisible to everything that
\ reads the map by offset. The copy loop now asks the map about the SOURCE word
\ and, when it is recorded, records the destination - SNAP-RELOC:CARRY-SITE.
\ The delta is that routine emitted at every C-CALL emission site, and there are
\ five of them (postpone's two arms, `."`, `.\"` and the compile-mode main loop).
\ Seventeen instruction words each: the region-domain test is a SUB, a one-lane
\ LIT64 for REGION ($800000 needs a single shifted move-wide) and a CMP with its
\ B.CS; the index arithmetic is four; the map address is a two-lane LIT64 for
\ ADDRMAP-OFF (302240 needs two) plus two ADDs; the bit is an LDRB, an LSRV and
\ an ANDI; then the CBZ and the BL to the recorder. The exact-CODELEN ratchet
\ measured the candidate at 118636 (was 118296), +340 = 5 * 17 * 4. Floor follows
\ from the same number: 7704 -> 8044, inside the same 16 KiB page, so the text
\ pad absorbs it and neither MACOS-SIGNATURE nor MACOS-TOTAL moves.
\ 2026-08-11, a failed defer/is says what went wrong (dot
\ habu-a-failed-defer-b83bcfa5): the four source-misuse causes used to share
\ C-DEFER-DIE-TOKEN, which wrote the offending token and nothing else, so a
\ build that tripped one printed a bare `HOOK` and exited 70. Each cause now
\ writes its own message, and the not-found case adds the using-import hint.
\ Two parts to the delta. The baked strings are 34 + 31 + 29 + 30 + 72 = 196
\ bytes, but `BYTES,` pads each label's data up to the next instruction word,
\ so they occupy 36 + 32 + 32 + 32 + 72 = 204. The emitted code replaces
\ C-DEFER-DIE-TOKEN's seven words (a three-word write of the token with its
\ two-word syscall, then the code and the branch) at four sites, 4 * 7 * 4 =
\ 112 bytes, with DEFER-DIAG's words: DIE-HEAD is three five-word writes
\ (message, token, newline), so DIE-MSG is seventeen words at three sites and
\ DIE-NOT-FOUND is twenty-two, (3 * 17 + 22) * 4 = 292. The exact-CODELEN
\ ratchet measured the candidate at 119020 (was 118636), +384 = 204 + 292 -
\ 112. Floor follows from the same number: 8044 -> 8428, inside the same 16 KiB
\ page, so the text pad absorbs it and neither MACOS-SIGNATURE nor MACOS-TOTAL
\ moves.
\ 2026-08-11, the AOT capture window's DATA content is baked (dot
\ habu-bake-the-aot-7ececce8): EM-AOT-RELOC-DATA reserved the span as zeroed
\ anon-mmap and copied nothing, on the reading that a REPL window is all
\ allot/variable and therefore all zero. It is not - a TRUST row's name and
\ signature are `s"` literals interned into the DP heap - so the window now
\ travels as bytes in its own baked section.
\ THE DELTA IS MOSTLY THE CONTENT, not code. The window measured 5726 bytes and
\ that is the section's whole size; the remaining 226 are the two new boot
\ routines (EM-AOT-COPY-DATA and EM-AOT-TRAP-XTCELLS), the named
\ "hb: AOT defer-unset missing" die, the declared-cell count cell and the u16
\ offsets table beside it, with that surface owned by package AOT-WINDOW. A
\ declared address cell inside the window is zeroed
\ rather than baked - its value is a code address in the BUILDING host, and
\ baking one made two builds of identical source differ by 34 bytes (2 of cell
\ plus the 32-byte signature that follows), which is the fixpoint this row is
\ measured at. The exact-CODELEN ratchet measured the candidate at 124964 (was
\ 119020), +5944 = 5726 of content + 218 of code and tables. Floor follows from
\ the same number: 8428 -> 14372, still inside the same 16 KiB page, so the text
\ pad absorbs it and neither MACOS-SIGNATURE (1295) nor MACOS-TOTAL (148855)
\ moves.
\ 2026-08-11 macOS 124964 -> 125156 (+192): the region-protection flip's extent
\ moved out of the LPROT body and became the caller's argument, so each call site
\ now carries its own one-instruction length operand. The gate's own map measured
\ the candidate at 125156; the derivation is exactly 49 EMITTED call sites at 4
\ bytes each, less the one 4-byte instruction removed from the body: +196 - 4.
\ FORTY-NINE, not the 48 sites in the source - two builder words that carry a
\ flip are themselves emitted more than once (one three times, one twice, 46
\ singletons), which is pre-existing structure the change does not touch and is
\ the whole reason the arithmetic looks off by one site.
\ Floor follows from the same number - MACOS-MODEL-FLOOR is (CODE-OFF +
\ CODE-TEXT) mod 16 KiB - so 14372 -> 14564, the same +192, still inside the same
\ 16 KiB page. The text pad absorbs it (2012 -> 1820, read off the candidate's own
\ map, where header 4096 + text 125156 + pad 1820 lands exactly on the eighth
\ 16 KiB page), so neither MACOS-SIGNATURE
\ (1295) nor MACOS-TOTAL (148855) moves and `FILE-SIZE bin/hb` is unchanged,
\ which is exactly why test/gate-build-size.f stayed green and this row did not:
\ the whole-file ratchet measures the page-rounded container and this one
\ measures the bytes.
\ 2026-08-11 the definer-facing registrar split (dot
\ habu-make-trust-refuse-cc8e19de) then moved it 125156 -> 125160. All 4 bytes
\ are one baked keyword string: the engine's publish tail and pre-trust defer
\ drain now resolve `trust-decl` instead of `trust`, and `BYTES,` pads a baked
\ string to a 4-byte boundary, so 5 bytes padded to 8 became 10 padded to 12.
\ Measured against ITS OWN base at the byte fixpoint on both trees with
\ HABU_ENGINE_SIZE_MAP=1 and reconciled with zero residue: dictionary-code
\ 6500 -> 6504 was the only emitter row that moved, and container/text-pad
\ absorbed it exactly. No instruction was added - C-FIND-TRUST-DECL is the old
\ C-FIND-TRUST renamed, and its two length immediates are MOVZ either way.
\ THE TWO DELTAS COMPOSE BY ADDITION and the composition was PREDICTED before it
\ was measured, which is what makes the row's arithmetic checkable rather than
\ fitted: the extent argument is 49 call sites of emitter text and the registrar
\ split is one baked string, so they touch disjoint bytes and neither moves the
\ other's rows. Predicted 125156 + 4 = 125160, floor 14564 + 4 = 14568, pad
\ 1820 -> 1816 (header 4096 + text 125160 + pad 1816 still lands on the eighth
\ 16 KiB page, so MACOS-SIGNATURE 1295 and MACOS-TOTAL 148855 are unchanged and
\ FILE-SIZE bin/hb stays 148855). The gate's candidate then confirmed it.
\ The linux row below is owed a linux-host re-measure for both deltas.
\ 2026-08-11 macOS 125160 -> 125412 (+252): the code-region protection flip
\ became a WINDOW - open it over the addresses a bracket will write, close
\ exactly what was opened, grow it when emission runs past its end (dot
\ habu-narrow-the-code-291b2cef). Predicted from the source before the candidate
\ was measured and then reconciled instruction class by instruction class against
\ the two binaries with ZERO residue - 63 instructions, 252 bytes:
\   -96 MOVZ  the 48 EMITTED flip sites each lose `2 x MOVZ,` and `1 REGION
\             LIT64,`. Forty-eight EMITTED, not 48 source sites: that is the same
\             multiplicity the +192 row above measured as 49, of which the boot
\             whole-region flip is the one left unconverted.
\   +23       one length instruction at each OPEN site (21 ADDI + 2 ADD - the
\             bulk publication and the AOT blob pass their own span). The 25
\             CLOSE sites gain nothing: a close reads the window it is closing.
\   +10 ADDI  the length each PROT:RESERVE site puts in x1 (11 emissions, one of
\             which is a MOVZ constant).
\   +66       11 emitted PROT:RESERVE expansions of 6 instructions. Eleven from
\             seven source sites because C-STORE-NAME is an emit-time macro
\             pasted at three definers and two more repeat.
\   +46       the three window bodies: PROT:LOPEN 20, PROT:LCLOSE 8,
\             PROT:LGROW 18 = +184 bytes, all in primitives/protect.
\   +11       LCEMIT's 3-instruction window check and 8-instruction miss path,
\             +44 bytes, all in primitives/cemit.
\   +2        EM-SNAPSHOT-RX-FLUSH clears the window cell, +8 bytes in
\             main/startup.
\   +7  MOVZ  inside those new bodies and sites.
\ Floor follows by the same +252 (MACOS-MODEL-FLOOR is (CODE-OFF + CODE-TEXT)
\ mod 16 KiB): 14568 -> 14820, still the eighth 16 KiB page. The text pad absorbs
\ it exactly, so MACOS-SIGNATURE (1295) and MACOS-TOTAL (148855) do not move and
\ test/gate-build-size.f stays green on its own row - which is why BOTH ratchets
\ had to be read: the whole-file one measures the page-rounded container and this
\ one measures the bytes.
\ The linux row below is owed a linux-host re-measure for this delta.
\ 2026-08-12 macOS 125412 -> 125560 (+148): the engine publishes its own number
\ reader as the primitive `num-parse` (dot habu-record-the-engine-79c570ed), so a
\ checked stage that has to know what cell a literal spelling stands for asks the
\ routine the interpret and compile dispatches already call at LNUM instead of
\ decoding the spelling a second time. MEASURED with HABU_ENGINE_SIZE_MAP=1 on
\ both trees at the byte fixpoint and reconciled with zero residue, two rows:
\   +100  primitives/base, ENGINE-EMIT:BNUMPARSE's own emitted body - 25
\         instructions. Two pop the address and the length into the registers
\         LNUM is entered with, one zeroes the float register the routine writes
\         only after it has chosen a base, one is the call, six turn the two
\         answers into Habu flags (CMPI, CSET and the negate that makes -1), two
\         AND the value and the float flag with the number flag so a spelling the
\         routine declined answers nothing at all, six push the three answers,
\         and five are FPRIM's frame and return - the prim is framed rather than
\         leaf because its body branches with link.
\   +48   seed-dictionary, the one 48-byte record (layout.f DREC) the new name
\         takes. `num-parse` is nine bytes, inside the 16-byte inline-name band
\         (DNAME-INL), so the dictionary-code region does not move and no name
\         bytes are emitted beside the record.
\ Floor follows from the same number (MACOS-MODEL-FLOOR is (CODE-OFF +
\ CODE-TEXT) mod 16 KiB): 14820 -> 14968, still the eighth 16 KiB page. The text
\ pad absorbs it exactly - 1564 -> 1416, read off the two maps - so
\ MACOS-SIGNATURE (1295) and MACOS-TOTAL (148855) do not move and `FILE-SIZE
\ bin/hb` is still 148855.
\ The linux row below is owed a linux-host re-measure for this delta too.
\ 2026-08-12 re-measured live at the macOS byte-fixpoint for the AOT capture
\ format widening (dot habu-widen-the-aot-089f5faf). Every offset in the baked
\ AOT frame moves u16 -> u32, which is what lifts the format's hard 64 KiB blob
\ ceiling: call-site rows 4 -> 8 bytes, compact dict records 16 -> 20, and the
\ DATA-site, CODE-site and window declared-cell offset lists 2 -> 4 bytes each.
\ On the metabuild REPL window (217 call sites, 115 records, 142 DATA sites, 3
\ CODE sites, 1 declared cell) that is +1620 bytes of baked table; the boot
\ walkers get SMALLER by about 60, because a u32 offset loads with one LDRW where
\ the u16 pair needed LDRB, LDRB, LSL and ORR. Composed: CODELEN 125560 ->
\ 127120 (+1560), all of it in aot-seed. Unlike every delta above it, this one
\ does not fit the pad: floor distance 14968 + 1560 = 16528, so header + code
\ takes one more 16 KiB __TEXT page (floor 14968 -> 144), the signature gains
\ four code-directory hash slots (1295 -> 1423), and the whole file moves 148855
\ -> 165367. MACOS-DATA-CONST and MACOS-LINKEDIT are unchanged, the model sum
\ reconstructs 165367 exactly, and MACOS-TOTAL still equals
\ BUILD-SIZE:BASELINE-MACOS in test/gate-build-size.f.
\ The Linux rows below are owed a linux-host re-measure for this delta too.
\ 2026-08-12 re-measured live at the macOS byte-fixpoint for the second half of
\ the same dot: an out-of-line name (past DNAME-INL) is no longer refused by the
\ capture, so EM-AOT-REGISTER-RECS gains the arm that points such a record's [24]
\ cell at the baked name pool instead of copying bytes into the record - four
\ instructions (test the flag, branch, store, join). CODELEN 127120 -> 127136
\ (+16), floor distance 144 -> 160. The extra sixteen bytes stay inside the 16 KiB
\ __TEXT page the widening just moved into, so MACOS-SIGNATURE (1423) and
\ MACOS-TOTAL (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ 2026-08-12 re-measured live at the macOS byte-fixpoint for the third piece of
\ the same dot: the named code-site row and its boot arm. The format gains a
\ fifth relocation class - a code-address chain paired with the NAME of the word
\ whose entry belongs in it - so AOT-XTSITE:PATCH-CHAINS joins the seed: a row walk,
\ an LFIND, the sealed-WID gate, the same four-lane immediate rewrite the two
\ rebase passes use, the address-map mark, and a named fd-2 refusal with its
\ message bytes. CODELEN 127136 -> 127508 (+372), floor distance 160 -> 532.
\ Still inside the same 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and
\ MACOS-TOTAL (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ 2026-08-13 re-measured live at the macOS byte-fixpoint for the definer-kind
\ stamp (dot habu-fold-a-named-052f4c4b): a record now says which definer made
\ it, so the two definers that emit a push-only body stamp their record and the
\ one writer that replaces such a body clears the stamp, and the AOT seed's boot
\ expander puts the pair back. Fourteen instructions, all of them
\ read-modify-write of the flags cell or its reconstruction: EMIT-CREATE 3 (LDR,
\ ORRI, STR), C-CONSTANT 3 (the same three), DOESPATCH:EMIT 4 (LDR, the clearing
\ mask, AND, STR - one instruction for the mask because $FFF3FFFFFFFFFFFF is one
\ run of ones and ENC-LIT takes the MOVN form), EM-AOT-REGISTER-RECS 4 (LSRI,
\ ANDI, LSLI, ORR). CODELEN 127508 -> 127564 (+56), floor distance 532 -> 588.
\ Still inside the same 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and
\ MACOS-TOTAL (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ 2026-08-14 re-measured live at the macOS byte-fixpoint for the field-accessor
\ conversion (dot habu-the-reader-re-a65e56e5): src/core/structures.f's +FIELD /
\ PTR-FIELD: / CFIELD: stop wearing two routines under one colon - each parses its
\ own name, bakes the offset into a generated ordinary colon accessor and evaluates
\ it through sumtype.f's TDECL-EVAL-XT boundary instead of `create ... does>`.
\ No emitter changed, so no compile or primitive region moves: the whole delta is
\ the AOT seed's captured record and name pool for the file's new definition set,
\ aot-seed 30512 -> 30520 (+8), measured by diffing the two HABU_ENGINE_SIZE_MAP
\ dumps region by region (every other row byte-identical). CODELEN 127564 ->
\ 127572, floor distance 588 -> 596. The text pad absorbs it (15796 -> 15788)
\ inside the same 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL
\ (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed a linux-host re-measure for this delta too.
\ 2026-08-14 re-measured live at the macOS byte-fixpoint for the pre-window
\ elimination (dot habu-aot-pre-window-0b01043c): the compile-mode inliner now asks,
\ per candidate body word, whether an address chain that word starts is one an open
\ AOT capture window could describe, and declines to copy the body if it is not.
\ Diffed region by region against the two HABU_ENGINE_SIZE_MAP dumps; four rows move
\ and every other row is byte-identical.
\   dictionary-code   6524 -> 6728  (+204): the EM-AOTWINQ routine, emitted once.
\   compile/keywords 10336 -> 10368  (+32): four of C-CALL's five emission sites,
\                                          two instructions each (BL + CBNZ).
\   compile/call        692 -> 700    (+8): its fifth emission site, the same two.
\   aot-seed          30520 -> 30516   (-4): NOT the capture. Every capture number is
\     unchanged (blob 18060, recs 115, call sites 217, name pool 953, DATA sites 142,
\     code sites 3, declared cells 1, boot-run 30), which is the measurement saying no
\     production body was declined - one that would be declined ends the build today.
\     What moved is the window's DATA span, 5729 -> 5726, because the new metabuild
\     definitions allot 27 bytes before the window opens and something inside the
\     window rounds DP up to eight. Falsified directly: a bare `create PW-PAD 27
\     allot` before CAPTURE-REPL on the UNCHANGED engine source reproduces d0, d1 and
\     the 5726 span byte for byte. Padded to four, the 3 bytes are this row's 4.
\ CODELEN 127572 -> 127812 (+240), floor distance 596 -> 836. The text pad absorbs it
\ (15788 -> 15548) inside the same 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and
\ MACOS-TOTAL (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed this +240 as well.
\ 2026-08-14 re-measured live at the macOS byte-fixpoint for the protection bands
\ (dot habu-narrow-the-boot-9637c873): the region's one write window becomes three
\ narrow bands - dictionary records, control-flow stack, code - each flipped only
\ over the pages a bracket declares. Diffed region by region against the two
\ HABU_ENGINE_SIZE_MAP dumps of THIS tree (not carried over from the pre-window
\ base it was first measured on); nine rows move and every other row is identical.
\   primitives/protect   460 -> 1048 (+588): the span-declaration router, the two
\     band helpers and the control-flow entry, against the old open/close/grow trio.
\   dictionary-code     6728 -> 6844 (+116): `immediate` and the does> patch move to
\     the stateless flip, and the AOT seed's captured set follows them.
\   interpret/define    9980 -> 10060  (+80): one declaration at each definer.
\   compile/exit        2472 -> 2496   (+24) and compile/semi 6232 -> 6252 (+20):
\     LBCHAIN's per-link declaration and the body length declared where it is written.
\   main/startup        6360 -> 6380   (+20): the four extra band cells cleared at the
\     whole-region RX flush.
\   interpret/colon      664 -> 680    (+16): the record and control-flow declarations.
\   primitives/cemit     144 -> 156    (+12): LCEMIT's test becomes a RANGE test.
\   primitives/qualify-def 2332 -> 2344 (+12) and primitives/base 14676 -> 14680 (+4).
\ CODELEN 127812 -> 128704 (+892), floor distance 836 -> 1728. The text pad absorbs
\ it (15548 -> 14656) inside the same 16 KiB __TEXT page, so MACOS-SIGNATURE (1423)
\ and MACOS-TOTAL (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed this +892 too, and the standing
\ LINUX-REGION-BUDGETS primitives/protect row with them.
\ 2026-08-14 re-measured live at the macOS byte-fixpoint for the every-boot AOT
\ seed (dot habu-decide-arm-the-5234727b): a boot's engine prefix and its user
\ program became two top-level source streams, so "source exhausted" is reached at
\ the end of the prefix - where the seed now runs - and the user stream is
\ installed there. Diffed region by region against the two HABU_ENGINE_SIZE_MAP
\ dumps; two code rows move and every other row is byte-identical.
\   compile/exit  2496 -> 2520 (+24): EM-COMPILE-EXIT. The arm-cell test is gone
\                                     (-8, two instructions) and the user-stream
\                                     install arrived (+32, eight: load the end
\                                     cell, branch out when it is 0, clear it,
\                                     carry INE into INP, store the new end, jump
\                                     to LMAIN).
\   main/startup  6380 -> 6376  (-4): C-SOURCE. The interactive-entry arm store is
\                                     gone (-8, movz + str) and the shared
\                                     cold-prefix routine publishes the prefix end
\                                     as INE (+4, one str). The baked-source
\                                     engines' matching store is not in THIS
\                                     engine, which emits the stdin path only.
\ CODELEN 128704 -> 128724 (+20), floor distance 1728 -> 1748. The text pad absorbs
\ it (14656 -> 14636) inside the same 16 KiB __TEXT page, so MACOS-SIGNATURE (1423)
\ and MACOS-TOTAL (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed this +20 as well.
\ 2026-08-14 re-measured live at the macOS byte-fixpoint for the seed gate's own
\ lookup (dot habu-return-the-record-9c9b1731): LFIND publishes the dictionary
\ record it matched, and the AOT boot gate reads that record instead of scanning
\ the whole dictionary for one whose code cell matches. Diffed region by region
\ against the two HABU_ENGINE_SIZE_MAP dumps; three code rows move and every other
\ row is byte-identical.
\   primitives/find      1260 -> 1284 (+24): EMIT-FIND, six instructions. Three
\                                     zero x5 on the three miss exits (FIND-NEND,
\                                     FIND-QBAD, FIND-MISS), one parks the linear
\                                     scan's match in x17 (that loop's cursor IS
\                                     x5), one puts it back at FIND-FOUND, and one
\                                     is the RET that FIND-FOUND and FIND-MISS used
\                                     to share and no longer can. The hash probe's
\                                     own match - the path that answers nearly every
\                                     lookup - gains nothing: x5 already holds the
\                                     record when it returns.
\   primitives/find-used  904 ->  912  (+8): EMIT-FIND-USED, two instructions, the
\                                     same split: the hit path gets its own RET and
\                                     the shared miss exit zeroes x5.
\   dictionary-code      6844 -> 6916 (+72): EM-AOTWIDGATE, 108 -> 180 bytes, and
\                                     the lookup half of it is a WASH. The scan cost
\                                     nine instructions (two to set the cursor, seven
\                                     for the loop body) and the checks that replaced
\                                     it cost nine as well - null, inside the record
\                                     array (subtract the base, multiply DREC by
\                                     NDICT, one unsigned compare for both ends), and
\                                     [0] equal to the xt LFIND returned. The whole
\                                     +72 is the fail-closed exit those checks needed:
\                                     eight instructions of write-and-exit (32 bytes,
\                                     each SYS being movz x16 + svc) where the old
\                                     routine simply fell through to the epilogue with
\                                     the guard unasked, plus its 38-byte diagnostic
\                                     padded to 40 and emitted beside the routine
\                                     rather than in the shared message pool.
\ CODELEN 128724 -> 128828 (+104), floor distance 1748 -> 1852. The text pad absorbs
\ it (14636 -> 14532) inside the same 16 KiB __TEXT page, so MACOS-SIGNATURE (1423)
\ and MACOS-TOTAL (165367) do not move and `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed this +104 too, split the same three ways.
\
\ 2026-08-14, dot habu-reach-the-seed-d1326596: every reference into the AOT
\ payload section stops using ADR, and goes through habu2.f IMGREF:TADR, - a
\ movz/movk offset, a load of the boot's text base and an add, four words where
\ ADR, was one. The section also moves after the baked source, so the ADR, that
\ reaches LSRC measures the ENGINE code half and nothing else.
\ MEASURED by diffing two HABU_ENGINE_SIZE_MAP=1 captures; the +360 of new sites
\ is one site times twelve each, against an -8 the same change pays back:
\   compile/exit      2520 -> 2856 (+336): the 28 sites in EM-SEED-AOT and
\                                     everything it inlines - COPY-BLOB,
\                                     REGISTER-RECS, PATCH-SITES, VALIDATE,
\                                     RELOC-DATA/CODE, COPY-DATA, TRAP-XTCELLS,
\                                     PATCH-CHAINS, BOOTRUN. 28 x 12.
\   dictionary-code   6916 -> 6940  (+24): EMIT-AOT-PROT-RESTORE's two sites.
\   main/startup      6376 -> 6368   (-8): the startup region SHRINKS. Its two
\                                     LSRC readers keep ADR, on purpose - that
\                                     ADR, is what measures the engine code half
\                                     - and the snapshot-presence test now reads
\                                     the image-end label directly (movz/movk)
\                                     instead of rebuilding the length from LSRC
\                                     plus the padded source (adr/sub/movz/add).
\ CODELEN 128828 -> 129180 (+352), floor distance 1852 -> 2204. The text pad
\ absorbs it again (14532 -> 14180) inside the same 16 KiB __TEXT page, so
\ MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) still do not move. The row
\ ORDER changed with the emit order (baked-source now precedes aot-seed); the
\ manifest looks rows up by name, so only the two numbers below move.
\ The Linux rows below are owed this +352 too.
\
\ 2026-08-15, dot habu-clear-the-addr-7595039c: EM-P2-START, the width-aware
\ recompile's entry, rewinds CP to the colon entry and now clears both relocation
\ maps over the span pass 1 emitted, so pass 1's records cannot describe words
\ pass 2 refills. MEASURED by diffing two HABU_ENGINE_SIZE_MAP=1 captures; the
\ whole delta is one region:
\   compile/semi      6252 -> 6596 (+344): EM-P2-START is EMITTED TWICE (the
\                                     publish gate's trigger and the standalone
\                                     trigger site further down habu2.f), and each
\                                     copy grows by 172 - two bit-clear loops
\                                     (SNAP-RELOC:CLEAR-SPAN,: 19 instructions
\                                     plus the map base's LIT64, one word for
\                                     CALLMAP-OFF and two for ADDRMAP-OFF, so 80
\                                     and 84 bytes) and the 8 bytes of span
\                                     arithmetic that replaced the bare CP load.
\ The habu1.f half of the same change is a pure factoring: NPUBWIN's
\ CLEAR-CALLMAP-SPAN became one call to the shared emitter, and a build with only
\ that hunk applied is BYTE-IDENTICAL to the baseline engine, which is how the
\ +344 is known to be the fix and nothing else.
\ CODELEN 129180 -> 129524 (+344), floor distance 2204 -> 2548. The text pad
\ absorbs it (14180 -> 13836) inside the same 16 KiB __TEXT page, so
\ MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not move and
\ `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed this +344 on compile/semi.
\ 2026-08-15 re-measured live at the macOS byte-fixpoint for the capture-window
\ arming word (dot habu-seed-the-chain-e98b03d4): src/habu/aot-arm.f becomes the
\ one writer of AOT-WINDOW:D0-CELL/B0-CELL and joins the stdin metabuild host
\ ahead of aot-capture.f, whose own AOT-CELL! (its last caller gone) is deleted.
\ Both files are HOST-only - neither is baked - so nothing in the engine's own
\ text moves. Diffed region by region against two HABU_ENGINE_SIZE_MAP dumps, one
\ of them a control build of master in a scratch export that reproduced the
\ pre-change engine byte for byte (sha 799f5601...): TWO rows move and every
\ other row is byte-identical.
\   aot-seed        30516 -> 30520 (+4): NOT the capture. Instrumenting
\     src/habu/stdin.f CAPTURE-REPL in both trees printed every captured quantity
\     and eight of the nine are identical (blob 18060, recs 115, call sites 217,
\     name pool 953, DATA sites 142, code sites 3, xt sites 0, declared cells 1).
\     The ninth is the window's DATA span, 5726 -> 5730, which the new host
\     definitions move because they allot before the window opens. Falsified on
\     the control tree with a change that is not this one: renaming aot-capture.f's
\     pre-window TRUST row AOT-CELL! to AOT-CELL2! - one byte of interned name -
\     moves the same span to 5725 and leaves the other eight numbers alone.
\   container/text-pad 13836 -> 13832 (-4): the pad absorbs it.
\ CODELEN 129524 -> 129528 (+4), floor distance 2548 -> 2552. Inside the same
\ 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not
\ move and `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed this +4 as well.
\ 2026-08-15, the captured-wid rebase (dot habu-rebase-captured-wids-54dec421).
\ The seed stopped registering a captured record under the wordlist id it had in
\ the CAPTURING process and started rebasing it into the booting engine's own wid
\ space, refusing one the capture window does not contain, and sealing the
\ wordlists that window sealed. Attribution is a region-by-region diff of two
\ HABU_ENGINE_SIZE_MAP dumps, the control being a scratch export of master that
\ reproduced the pre-change engine byte for byte (sha d151fbac...): THREE rows
\ move and every other one of the 49 is byte-identical.
\   compile/exit    2856 -> 3272 (+416): the boot pass itself - the per-wid
\     rebase in the record loop, the window-seal loop, the named refusal, and the
\     single WIDN advance that replaced the per-record one.
\   aot-seed        30520 -> 30536 (+16): the window's wid base, its span and the
\     sealed-id count, measured after alignment. The sealed-id TABLE is empty in
\     this engine - the metabuild's REPL window creates no wordlist and so seals
\     none - so only the cells show. A chain capture fills it with 125 rows.
\   container/text-pad 13832 -> 13400 (-432): the pad absorbs the whole of it.
\ CODELEN 129528 -> 129960 (+432), floor distance 2552 -> 2984. Inside the same
\ 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not
\ move and `FILE-SIZE bin/hb` is still 165367.
\ The Linux rows below are owed this +432 as well.
\ 2026-08-16 the call site's callee wordlist (dot habu-seed-call-site-9d7d8e72),
\ measured live at the byte fixpoint against a control build of master dc18bbca.
\ Four rows move and every other one of the 49 is byte-identical:
\   aot-seed        30536 -> 31404 (+868): the seed's four-clause scope resolve,
\     the [T0, T0+span) test that keeps the sealed-WID gate off a wordlist the
\     seed itself just created, and the two named boot diagnostics that replaced
\     the silent $51 exit.
\   compile/exit     3272 -> 3576 (+304): the same pass, emitted inline a second
\     time under EM-COMPILE-EXIT.
\   primitives/find-wl 0 -> 516 (+516): the one-wordlist find, now a labelled
\     routine two callers share.
\   primitives/base 14680 -> 14204 (-476): `search-wl`'s body, which is what
\     moved into that routine.
\ CODELEN 129960 -> 131172 (+1212), floor distance 2984 -> 4196. Inside the same
\ 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not
\ move and `FILE-SIZE bin/hb` is still 165367 (measured).
\ The Linux rows below are owed this +1212 as well.
\ 2026-08-16 the sealed-WID gate's four layers (dot habu-seed-call-site-9d7d8e72),
\ measured live at the byte fixpoint against a control build of master 9c9a43de
\ with the same HABU_ENGINE_SIZE_MAP run. Three rows move and every other one of
\ the 50 is byte-identical:
\   dictionary-code  6940 -> 7108 (+168): EM-AOTWIDGATE stops asking "is this wid
\     protected" and decides in four layers instead - the window test against the
\     latched T0, the two engine-reserved wordlists, the protected bit, and the
\     namespace-row scan for the package's public slot - plus the callee name the
\     reject now writes on fd 2 before exit 84.
\   compile/exit     3576 -> 3544 (-32): the call-site pass gave its own
\     in-window exemption up to that routine, so the pass emitted inline under
\     EM-COMPILE-EXIT lost the test it used to carry.
\   runtime          9256 -> 9252 (-4): EM-AOT-REGISTER-RECS latches T0 into the
\     new AOT-WINDOW:T0-CELL and the surrounding register use settles four bytes
\     shorter.
\   container/text-pad 12188 -> 12056 (-132): the pad absorbs the whole net.
\ CODELEN 131172 -> 131304 (+132), floor distance 4196 -> 4328. Inside the same
\ 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not
\ move and `FILE-SIZE bin/hb` is still 165367 (measured).
\ The Linux rows below are owed this +132 as well.
\ 2026-08-16 the breakpoint handler's return-address line (dot
\ habu-merged-engine-nmigrate-c970bf04), measured the same way against the
\ four-layer-gate build. Two rows move:
\   runtime          9252 -> 9296 (+44): C-BP-PRINT-HIT reads the interrupted
\     x30 out of the same mcontext the pc comes from and prints it under its own
\     header, so a breakpoint in a seeded engine can name its caller.
\   aot-seed        31404 -> 31408 (+4): the "habu-bp-lr:" bytes, emitted with
\     the handler's other message labels.
\   container/text-pad 12056 -> 12008 (-48): the pad absorbs the whole net.
\ CODELEN 131304 -> 131352 (+48), floor distance 4328 -> 4376. Inside the same
\ 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not
\ move and `FILE-SIZE bin/hb` is still 165367 (measured).
\ The Linux rows below are owed this +48 as well.
\ 2026-08-16 the does>-clause dictionary record (dot
\ habu-merged-engine-nmigrate-c970bf04), measured against a control build of the
\ same tree without it. Two emitter rows move:
\   compile/keywords 10368 -> 10920 (+552): J-DOES now assembles the `adr` that
\     computes the clause entry, writes the clause's name out of line at CP, and
\     builds its 48-byte record - with the two capacity exits (dict slot, code
\     region) that reach for C-DIE-DICT-FULL / C-DIE-CODE-FULL.
\   compile/semi     6596 -> 6664 (+68): the clause record's own length at the
\     flush, and its count plus hash-index row at the publish.
\   container/text-pad 12008 -> 11388 (-620): the pad absorbs the whole net.
\ CODELEN 131352 -> 131972 (+620), floor distance 4376 -> 4996. Inside the same
\ 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not
\ move and `FILE-SIZE bin/hb` is still 165367 (measured).
\ The Linux rows below are owed this +620 as well.
\ 2026-08-16 the seeded DATA window's alignment (dot
\ habu-merged-data-window-b8fec035), measured against a control build of the
\ same tree without it. One emitter row moves:
\   compile/exit 3544 -> 3556 (+12): EM-AOT-RELOC-DATA advances DP to the
\     residue of the base the window was captured against, so the seed's delta
\     is a whole number of cells and every captured address keeps its alignment.
\   container/text-pad 11388 -> 11376 (-12): the pad absorbs it.
\ CODELEN 131972 -> 131984 (+12), floor distance 4996 -> 5008. Inside the same
\ 16 KiB __TEXT page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not
\ move and `FILE-SIZE bin/hb` is still 165367 (measured).
\ The Linux rows below are owed this +12 as well.
\ 2026-08-17 the seeded checker payload (dot
\ habu-seeded-words-invisible-c7505a49). The whole +3504 lands in aot-seed, and
\ it divides with no residue into three measured parts:
\   3452  the payload itself, read out of the built engine's own published cell
\         (`data-base AOT-SIG:LEN-CELL + @`): the REPL window's signature rows,
\         the strings they name, an empty type registry and the 56-byte table
\         that says where each of the three begins.
\      8  the length word EMIT-AOT-SEED bakes in front of it (AOT-SIG:LLEN).
\     44  AOT-SIG:PUBLISH,, the eleven instructions the seed runs to store that
\         address and that length into the two DATA cells: two TADR, (four words
\         each, being LOFF, + the text-base load + an add) plus LDR and two STR.
\ CODELEN 131984 -> 135488 (+3504), floor distance 5008 -> 8512. Still the eighth
\ 16 KiB __TEXT page ((CODE-OFF + CODE-TEXT) / 16 KiB is 8 both sides), so
\ MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not move and
\ `FILE-SIZE bin/hb` is still 165367 (measured).
\ The Linux rows below are owed this +3504 as well; a Linux window's payload is
\ its own REPL capture, so the number there is that host's to measure.
\ 2026-08-17 the seed's registry install and the region's new caps, same dot.
\ +160, and it divides between two commits with no residue. Each half was
\ measured by building that commit's own tree and diffing its
\ HABU_ENGINE_SIZE_MAP against its parent's, so every byte below names a row.
\   +72  `aot: install the seeded type registry at the seed point`
\        +52 compile/exit: AOT-SIG:INSTALL,, the thirteen instructions the seed
\            runs to resolve CK-AOT-REG-INSTALL and call it - one TADR, (four
\            words), MOVZ, BL, CBZ, BL, BLR, B, MOVZ, and a two-word SYS,.
\        +20 aot-seed: the name it resolves by, `CK-AOT-REG-INSTALL`, 18 bytes
\            in the emitted pool.
\   +88  `layout: size both region bands for the composite they hold`
\        +16 interpret/define, +16 primitives/find, +8 primitives/find-wl,
\        +8 primitives/find-used, +24 primitives/hash-index,
\        +16 primitives/qualify-def - the six phases that materialise DICT-CAP,
\        HIDX-SLOTS, HIDX-BYTES and HIDX:LOAD-MAX. A raised cap needs a second
\        instruction where one MOVZ carried the old one, which is the cost the
\        LIT64 conversion made honest rather than the cost it added.
\ CODELEN 135488 -> 135648, floor distance 8512 -> 8672. container/text-pad
\ 7872 -> 7712 absorbs the whole +160 inside the same eighth 16 KiB __TEXT page,
\ so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not move and
\ `FILE-SIZE bin/hb` is still 165367 (measured).
\ The Linux rows below are owed this +160 as well.
\ 2026-08-18 the window's DATA went sparse (dot
\ habu-census-the-captured-fe5f7c49). The engine used to bake its REPL window's
\ whole DATA span verbatim, and that span is a dictionary subrange: almost all of
\ it is `allot`ed room nothing has written. It now bakes the window's NON-ZERO
\ EXTENTS - a count, a table of (offset u32, length u32) rows, and their bytes in
\ row order - and the seed zeroes the span before laying them in. -5560, and it
\ divides between two rows with no residue:
\  -5692  aot-seed: the REPL window's verbatim span, out; its extent count, its
\         run table and its run bytes, in.
\   +132  compile/exit: AOT-WINDOW:ZERO-SPAN and AOT-WINDOW:APPLY-RUNS, in place
\         of the byte-at-a-time COPY-DATA they replace - an eight-byte store loop
\         with a byte tail, then a run walk with a running byte cursor.
\ CODELEN 135648 -> 130088, floor distance 8672 -> 3112. container/text-pad
\ 7712 -> 13272 absorbs the whole -5560 inside the same eighth 16 KiB __TEXT
\ page, so MACOS-SIGNATURE (1423) and MACOS-TOTAL (165367) do not move and
\ `FILE-SIZE bin/hb-host` is still 165367 (measured).
\ The PRODUCT is where the same change is worth megabytes rather than kilobytes:
\ its window is the REPL capture merged with the compiler chain's, whose span is
\ 1,531,045 bytes carrying 32 bytes of content in four cells. bin/hb goes
\ 3,649,399 -> 2,097,271, aot-seed 3,486,060 -> 1,949,388 (measured on the same
\ two builds), and the artifact the capture writes goes 3,454,098 -> 1,923,141.
\ The Linux rows below are owed this -5560 as well, split the same two ways.
130088 constant MACOS-CODE-TEXT   \ CODELEN: every emitter-phase row (baked-source incl.)
1423 constant MACOS-SIGNATURE     \ ad-hoc code signature SuperBlob (grows with CODELEN)
3112 constant MACOS-FLOOR-DIST    \ code above the 16 KiB floor: the page-recovery shave
165367 constant MACOS-TOTAL       \ = FILE-SIZE bin/hb = BUILD-SIZE:BASELINE-MACOS

\ Linux committed attribution, measured at the byte-fixpoint on 2026-07-19 (DGX
\ Spark, linux-arm64) after the shared PROT-GUARD:CALL span-guard fold (CODELEN
\ 142092 -> 136108), then advanced by the same +28-byte shared-engine-text delta
\ as macOS above (LP2VEXEC record, predicted 136108 -> 136136), then re-measured
\ live at the 2026-07-19 seal-guard fixpoint: the raw ffi-call nargs guard adds
\ 404 bytes, CODELEN 136136 -> 136540 (floor 968 -> 1372), confirming the +28
\ prediction; all 432 bytes since the fold fit inside the same 4 KiB page
\ padding (LINUX-TOTAL unchanged). Keep LINUX-TOTAL equal to
\ BUILD-SIZE:BASELINE-LINUX in test/gate-build-size.f.
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
\ 2026-07-21 re-measured at the merged fixpoint after the Mac's field-generation
\ landing (shared engine source, spark owes the Linux rows per the per-target
\ asymmetry): +48 of engine text, CODELEN 134860 -> 134908 (floor 3788 -> 3836);
\ same 4 KiB page, LINUX-TOTAL unchanged.
\ 2026-07-21 re-measured at the merged fixpoint after the Mac's structure-make
\ split landing (spark owes the Linux rows): +48 of engine text, CODELEN
\ 134908 -> 134956 (floor 3836 -> 3884); same 4 KiB page, LINUX-TOTAL unchanged.
\ 2026-07-21 re-measured at the merged fixpoint: the scalar/relocatable literal
\ split (dot habu-separate-scalar-and-dffe142e) shaves -1364 of engine text on the
\ structure-make base (CODELEN 134956 -> 133592; floor 3884 -> 2520; file unchanged) -
\ scalars now emit minimal MOVZ/MOVN+MOVK into x16, addresses keep the fixed x9
\ relocation chain. Region attribution: interpret/define -136, compile/keywords -760,
\ aot-seed -468.
\ 2026-07-21 re-measured at the merged fixpoint: the shared definer publication
\ path (dot habu-emit-one-shared-dae6cd61) emits the qualify/store bodies ONCE as
\ native BL routines instead of inlining them at 5/6 definers - on top of the
\ literal split, CODELEN 133592 -> 122124, floor 2520 -> 3340, whole file
\ 139456 -> 127168 (drops two 4 KiB pages). Region attribution in the rows.
\ 2026-07-21 re-measured at the handoff fixpoint (the Mac's final ENUM front-end
\ landing 9340f6a3; spark now owns all rows): CODELEN 122164, floor 3380,
\ file 127168. Region deltas in the rows.
\ 2026-07-22 re-measured at the atomic generated-declaration fixpoint.  The
\ declaration transaction's startup wiring, native registry primitives, and
\ dictionary, seed, and ahead-of-time publication paths add 380 bytes of
\ measured engine text:
\ CODELEN 122164 -> 122544 and floor 3380 -> 3760.  The same-page text pad
\ shrinks 716 -> 336, so the whole file remains exactly 127168.  Attribution:
\ main/startup +64, primitives/base +48, dictionary-code +172,
\ seed-dictionary +48, and aot-seed +48.
\ 2026-07-23 re-measured at the ENUM declaration-ownership fixpoint. The
\ declaration event and sealed type-name wiring add 16 bytes to aot-seed:
\ CODELEN 122544 -> 122560 and floor 3760 -> 3776. The same 4 KiB page still
\ contains __text, so the whole file remains exactly 127168.
\ 2026-07-25 product-field lifecycle ownership (habu-own-product-field-86660116
\ + the folded habu-type-field-owner-619ec6b5): the eight TYPE-FIELD-OWNER
\ checker axioms and the retired-token reject add 8 bytes to aot-seed, measured
\ at this lane's fixpoint on top of the measured master bbc9bf5e numbers:
\ CODELEN 122560 -> 122568 and floor 3776 -> 3784. The text pad absorbs the 8
\ bytes (320 -> 312), so the whole file remains exactly 127168.
\ 2026-07-25 checker declaration frames: the four CHECKER-DECL-FRAME primitive
\ axiom rows (START/PREPARE/ROLLBACK/RELEASE, each closed into the package
\ private wordlist with CLOSE-PRIVATE) add 16 bytes to aot-seed, measured at this
\ lane's fixpoint on top of the product-field lifecycle numbers above:
\ CODELEN 122568 -> 122584 and floor 3784 -> 3800. The text pad absorbs the 16
\ bytes (312 -> 296), so the whole file remains exactly 127168. The two build
\ fixes in the same commit move no engine bytes: src/habu/verify-source.f is not
\ part of the engine build at all, and src/core/checker.f is boot-time source the
\ engine reads at every launch (renaming a definition there was measured
\ byte-neutral for this row).
\ 2026-07-25 constructor-generation participant (package GENERATED-DECL-CTOR in
\ src/core/generated-declaration.f, dot habu-enum-generate-named-1f3261a3):
\ re-measured live at the linux-arm64 byte fixpoint (install --force run twice to
\ a byte-identical bin/hb, then HABU_ENGINE_SIZE_MAP=1 captured through the stdin
\ metabuild host and reconciled with zero residue). The whole delta is +8 bytes of
\ aot-seed (22500 -> 22508) - the participant's private axiom rows - so CODELEN
\ 122584 -> 122592 and floor 3800 -> 3808. The text pad absorbs the 8 bytes
\ (296 -> 288), so the whole file remains exactly 127168 and LINUX-TOTAL is
\ unchanged. src/core/enum-decl.f moves no engine bytes: it gains no definition,
\ only a call to the participant's gate. tools/decl-gen-probe.f and the four
\ test files are not part of the assembled
\ stage2 engine source.
\ cda6ec6d: compile/ops +288, dictionary-code +16, aot-seed -312; net -8, total 123072 unchanged.
\
\ 2026-08-05 THE LINUX DECOMPOSITION IS OWED A RE-MEASURE, AND SAYS SO.
\ Merge cd7bf8eb resolved these four rows to the pre-merge MASTER binary's numbers
\ (118420 / 3732 / 123072) while keeping the campaign's prose above them, which is
\ why that prose ends "the whole file remains exactly 127168". The rebuilt
\ integrated tip measures 127168, so the whole-file row was simply wrong and is
\ corrected here; BUILD-SIZE:BASELINE-LINUX carries the same number.
\
\ The DECOMPOSITION cannot be corrected the same way, and inventing it would be
\ the same defect again. The model reconstructs the total as
\ PAGE-UP(CODE-OFF + CODE-TEXT, 4 KiB) + LINUX-RW, so a measured total of 127168
\ pins CODE-OFF + CODE-TEXT only to the half-open 4 KiB page (122880, 126976] -
\ one equation, two unknowns. Neither surviving candidate describes this binary:
\ 118420 is master's engine before the campaign's text landed, and the campaign's
\ own 122592 predates master's owner-registry deletion, which removes engine text
\ (the per-region rows below still carry master's shape - no aot-owner row, and
\ protected-wid at 120 rather than 1540). Deriving a third number from the page
\ boundary would be page rounding wearing an attribution's clothes.
\
\ So CODE-TEXT and FLOOR-DIST are ZERO, which this manifest already means as
\ "unmeasured target, fail closed with the measured size to commit"
\ (test/gate-build-size.f header), and LINUX-MEASURED? gates the committed-row
\ self-checks the way HOST-REGION-BUDGETS-MEASURED? already gates the macOS
\ per-region rows. A Linux host running the candidate gate now reports the real
\ CODELEN to commit instead of a drift against a binary that no longer exists.
\ The per-region rows are left standing as the last measurement anyone took, so
\ that re-measure starts from a diff rather than from nothing.
0 constant LINUX-CODE-TEXT        \ OWED: unmeasured on the integrated tree
192 constant LINUX-RW             \ ELF read-write segment tail: DYNAMIC + GOT (ELF-RW-SZ)
0 constant LINUX-FLOOR-DIST       \ OWED: derives from CODE-TEXT, unmeasured with it
127168 constant LINUX-TOTAL       \ = FILE-SIZE bin/hb = BUILD-SIZE:BASELINE-LINUX

\ A zero CODE-TEXT row is this manifest's spelling of "unmeasured": there is no
\ committed decomposition to check against, and the rows that would check it stay
\ silent rather than assert something nobody measured.
: LINUX-MEASURED? ( -- bool )
   LINUX-CODE-TEXT 0 <> ;

\ --- Per-region __text budgets (dot habu-enforce-native-region-1003651b) -------
\ The whole-file (BUILD-SIZE) and __text-total (CODE-TEXT / SUM-TEXT) ratchets catch
\ aggregate growth, but a region that grows while a sibling shrinks nets zero at
\ the total and hides which emitter moved, and a lone region regression only
\ surfaces once it crosses a page. These committed rows decompose CODE-TEXT into
\ one budget per emitter phase (plus baked-source), measured same-commit at the
\ byte fixpoint (HABU_ENGINE_SIZE_MAP=1 -> tools/size-report.f). They sum to
\ LINUX-CODE-TEXT (RUN asserts it), so a bump/lowering here is the per-region
\ analogue of the CODE-TEXT ratchet: the owning change re-measures and updates
\ exactly the rows it moved, and the gate names any region whose measured size
\ drifts from its budget. Container regions keep their independent ceilings
\ (SUM-TEXT / floor-dist / the container rows above). macOS per-region budgets are
\ owed until a macOS host measures them (HOST-REGION-BUDGETS-MEASURED?), mirroring
\ the CODE-TEXT/census per-target asymmetry; the macOS whole-file and CODE-TEXT
\ ceilings above are untouched.
\ 2026-07-21 first live drift, re-measured at the merged fixpoint: the Mac's
\ structure-make split (+48 total) attributes as main/startup +16 (new load row)
\ and dictionary-code +32 (the generator's baked entry) - exactly the
\ attribution this ratchet exists to give.
: LINUX-REGION-BUDGETS ( [ ptr u8 n n -- ] -- ) {: q :}   \ typed-local-lint: allow-bare-local - q carries the row effect
   s" main/startup"            4792 q execute
   s" main/comment"             380 q execute
   s" interpret/colon"         752 q execute
   s" interpret/define"       10464 q execute
   s" interpret/string"        1148 q execute
   s" interpret/number"          48 q execute
   s" interpret/find"           132 q execute
   s" compile/adt"             2140 q execute
   s" compile/semi"            6892 q execute
   s" compile/local"            552 q execute
   s" compile/p2wide"          2460 q execute
   s" compile/keywords"       9916 q execute
   s" compile/literal"           36 q execute
   s" compile/ops"             2744 q execute
   s" compile/call"             628 q execute
   s" compile/undef"            924 q execute
   s" compile/die"              200 q execute
   s" compile/exit"            1868 q execute
   s" compile/eval-recover"     724 q execute
   s" main/underflow"           192 q execute
   s" primitives/base"        17604 q execute
   s" primitives/arity"         760 q execute
   s" primitives/extra"         568 q execute
   s" primitives/prof"          220 q execute
   s" primitives/float"         764 q execute
   s" primitives/cemit"         108 q execute
   s" primitives/cemitbl"       100 q execute
   s" primitives/capture"       156 q execute
   s" primitives/token"         104 q execute
   s" primitives/protect"       304 q execute
   s" primitives/protected-wid" 120 q execute
   s" primitives/flush"          72 q execute
   s" primitives/find"          952 q execute
   s" primitives/find-used"     520 q execute
   s" primitives/hash-index"    852 q execute
   s" primitives/number"        332 q execute
   s" primitives/top-hook"       68 q execute
   s" dictionary-code"         5016 q execute
   s" runtime"                 9464 q execute
   s" seed-dictionary"         8352 q execute
   s" aot-seed"               22156 q execute
   s" primitives/qualify-def"  2448 q execute
   s" primitives/store-def-name"   388 q execute
   s" baked-source"               0 q execute ;

variable RB-ACC
: RB-SUM-STEP ( ptr u8 n n -- ) {: na:ptr nu:n v:n :}
   v RB-ACC @ + RB-ACC ! ;
: LINUX-REGION-BUDGET-SUM ( -- n )
   0 RB-ACC ! [: RB-SUM-STEP ;] LINUX-REGION-BUDGETS RB-ACC @ ;

variable RB-Q-A  variable RB-Q-U  variable RB-Q-V  variable RB-Q-HIT
: RB-FIND-STEP ( ptr u8 n n -- ) {: na:ptr nu:n v:n :}
   na nu RB-Q-A @ RB-Q-U @ STR= if v RB-Q-V ! -1 RB-Q-HIT ! then ;

\ Page-crossing prediction, per target, from THAT target's own measured layout
\ (never inferred across targets): headroom = the __text growth the current text
\ segment absorbs before the file gains a page. macOS reads its 16 KiB __TEXT
\ floor, Linux its 4 KiB text floor; each uses only its own measured FLOOR-DIST.
: MACOS-PAGE-HEADROOM ( -- n )  MACOS-PAGE MACOS-FLOOR-DIST - ;
: LINUX-PAGE-HEADROOM ( -- n )  LINUX-PAGE LINUX-FLOOR-DIST - ;

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

\ The installed engine carries a whole engine and, in a seeded product, a chain
\ besides. Less than the committed total is an engine that is not this one.
: LIVE-CARRIES-ENGINE? ( -- bool )
   LIVE-ENGINE HOST-TOTAL >= ;

public

\ Committed CODE-TEXT (__text) row for whichever target is running (0 = unmeasured).
\ The gate's exact-CODELEN ratchet holds the candidate's measured SUM-TEXT to this.
: HOST-CODE-TEXT ( -- n )
   HB-TARGET-MACOS? if MACOS-CODE-TEXT exit then
   HB-TARGET-LINUX? if LINUX-CODE-TEXT exit then
   0 ;

\ Committed per-region __text budgets for the running target, applied to xt as
\ ( ptr u8 n budget -- ) per row. macOS is owed (no rows) until a macOS host
\ measures it; HOST-REGION-BUDGETS-MEASURED? gates live per-region enforcement.
: HOST-REGION-BUDGETS ( [ ptr u8 n n -- ] -- ) {: q :}   \ typed-local-lint: allow-bare-local - q carries the row effect
   HB-TARGET-LINUX? if q LINUX-REGION-BUDGETS then ;

: HOST-REGION-BUDGETS-MEASURED? ( -- bool )
   HB-TARGET-LINUX? LINUX-MEASURED? and ;

\ SOME committed budget for the named region on the running target, else NONE.
: HOST-REGION-BUDGET-FIND ( ptr u8 n -- option<n> ) {: qa:ptr qu:n :}
   qa RB-Q-A ! qu RB-Q-U ! 0 RB-Q-HIT !
   [: RB-FIND-STEP ;] HOST-REGION-BUDGETS
   RB-Q-HIT @ if RB-Q-V @ OPTION:SOME else OPTION:NONE then ;

\ Per-target page-crossing prediction, each from its own measured layout.
: PAGE-CROSS-REPORT ( -- )
   s" page-cross(macos): " type MACOS-PAGE-HEADROOM .
   s" bytes __text headroom to the 16 KiB __TEXT floor (measured macos layout)" type cr
   s" page-cross(linux): " type LINUX-PAGE-HEADROOM .
   s" bytes __text headroom to the 4 KiB text floor (measured linux layout)" type cr ;

\ Pure self-check (no build): each target's committed decomposition reconstructs
\ its whole file and page-floor shave, and the live installed engine carries at
\ least the committed total.
\ THE LIVE COUPLING IS DIRECTIONAL NOW, and the reason is what bin/hb became. The
\ rows below decompose the CAPTURE HOST - the engine the build emits first and the
\ one every number here was measured on - and the installed engine is that engine
\ plus a baked compiler chain whose payload moves with every compiler-source edit
\ (dot habu-seed-the-chain-e98b03d4). Equality here would be a payload ratchet. The
\ EXACT coupling did not weaken, it MOVED to where the host exists: the engine
\ build slice runs SIZE-ATTR:VALIDATE against hb-host on every build, which holds
\ SUM-ALL to that file byte for byte and every committed region row with it.
\ The committed-row self-check for a target whose decomposition is measured. A
\ target whose CODE-TEXT is owed has nothing here to check: its rows do not claim
\ to reconstruct anything, and the live coupling below still holds its whole-file
\ total against the running engine.
: LINUX-SELF-CHECK ( -- )
   LINUX-MEASURED? 0= if exit then
   LINUX-MODEL-SUM LINUX-TOTAL T=
   LINUX-MODEL-FLOOR LINUX-FLOOR-DIST T=
   LINUX-REGION-BUDGET-SUM LINUX-CODE-TEXT T= ;   \ per-region budgets decompose __text exactly

: RUN ( -- )
   T-RESET
   MACOS-MODEL-SUM MACOS-TOTAL T=
   MACOS-MODEL-FLOOR MACOS-FLOOR-DIST T=
   LINUX-SELF-CHECK
   LIVE-CARRIES-ENGINE? TTRUE
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

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
118084 constant MACOS-CODE-TEXT   \ CODELEN: every emitter-phase row (baked-source incl.)
1295 constant MACOS-SIGNATURE     \ ad-hoc code signature SuperBlob (grows with CODELEN)
7492 constant MACOS-FLOOR-DIST     \ code above the 16 KiB floor: the page-recovery shave
148855 constant MACOS-TOTAL       \ = FILE-SIZE bin/hb = BUILD-SIZE:BASELINE-MACOS

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
\ its whole file and page-floor shave, and the running target's committed total
\ equals the live installed engine. Any drift - a bigger engine, a stale row -
\ fails one of these, so the manifest cannot silently fall behind reality.
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

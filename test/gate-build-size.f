\ gate-build-size.f - committed size ratchet for the Habu-under-test candidate.
\
\ Machine-readable baseline manifest, mirroring the TRUSTED.md pattern: the
\ committed rows below are the contract, and the owning gate validates the
\ fresh artifact against them. One row per build target. The candidate build
\ and validate slices (test/gate-engine-lib.f) fail on any candidate larger
\ than its row, so growth needs a deliberate same-commit row bump here.
\ A smaller candidate reports STALE-BASELINE so the shrinking commit lowers
\ the row. A zero row is an unmeasured target and fails closed with the
\ measured size to commit.
\
\ Load after test/gate-common.f.

\ 2026-07-06 macOS bump 115831 -> 132343: TFAM 12 slice 3b pass-2 width-aware
\ transport lowering (habu2.f EM-COMPILE-P2WIDE dispatch, per-op width queries,
\ LP2COPY/LP2ROT/LP2RS emit helpers, width-aware locals carve/reference, and
\ the publish-path recompile trigger). The Linux row still holds the pre-3b
\ value; the next Linux lane run fails closed with the measured size to commit.
\ 2026-07-09 macOS bump 132343 -> 148855: TFAM 10 slice 3a native MATCH lowering
\ (habu2.f J-MATCH + EM-ADT-MATCH-FAM/VAR/OF + EM-MATCH-SEMI + the C-DIE-BAD-TAG
\ inline invalid-tag die, plus the CMBK/CMFR match-frame state and the tfl-match-
\ fam?/tfam-name$/bad-tag kwdata). Page-granular growth (same delta as slice 3b).
\ 2026-07-10 MERGE macOS bump 148855 -> 148855: type-families campaign merged into
\ fable. The merged engine = TFAM slice-3a/3b lowering (above) plus fable's set-check
\ checker-xt validation, BCHECKFETCH, undefined-word EVAL recovery (code 70), and the
\ engine-id/underflow-guard/boot-pin work; those fable fixes fit within the existing
\ 148855-byte page floor, so the macOS ratchet is unchanged on the merged fixpoint.
\ 2026-07-12 macOS measurement remains 148855 after the staged `;package`
\ migration removed the legacy keyword data/dispatch entry.
\ 2026-07-13 macOS bump 148855 -> 165367: certified-word underdepth gate (dot
\ habu-habu-certified-words-84e84eaf): DNAME-MIN-IN fold in LFIND, the
\ EM-INTERPRET-FIND depth guard + LMININ diagnostic leg, the publish-tail
\ min-in poke, the min-in-mark prim, the AOT compact-record min-in byte, and
\ ~14 new LARITY guard rows (evaluate/catch/ffi-call*/patch32/search-wl/
\ set-check/cp!/ndict!). ~1 KB of emitted code crossing the 16 KB page floor
\ plus the code-signature growth accounts for the page-granular delta. The
\ Linux row is untouched; its next lane run fails closed with the measured
\ size to commit.
\ 2026-07-14 Linux bump 102592 -> 147648: first Linux re-measure since 2026-07-04
\ - the merged engine adds the TFAM slice-3a/3b lowering, the type-family checker
\ + tok-imm? immediate model, and the certified-word underdepth gate; fixpoint x2
\ byte-identical on the Orin.
\ 2026-07-19 macOS 165367 -> 165367: MATCH dispatch B.cond slimming (dot
\ habu-slim-match-emitted-66941fb5) nets +28 bytes of engine text (CODELEN
\ 127832 -> 127860); it fits inside the same nine 16 KiB __TEXT pages, so the
\ page-rounded whole-file total is unchanged. See the exact CODE-TEXT/floor rows
\ in test/gate-size-attribution-test.f (macOS re-measured live; Linux pending a
\ next-fixpoint re-measurement).
\ 2026-07-19 macOS 165367 -> 165367: CASE OF dispatch B.cond slimming (dot
\ habu-branch-directly-on-4624d193) nets -12 bytes of engine text (CODELEN
\ 127536 -> 127524) by dropping the per-arm cset boolean in J-OF; it stays inside
\ the same nine 16 KiB __TEXT pages, so the page-rounded whole-file total is
\ unchanged. See the exact CODE-TEXT/floor rows in
\ test/gate-size-attribution-test.f (macOS re-measured live; Linux pending a
\ next-fixpoint re-measurement).
\ 2026-07-20 Linux 143552 -> 143552: JIT constant-push / binary-prep
\ de-duplication (dot habu-share-duplicated-jit) nets -272 bytes of engine text
\ (CODELEN 136088 -> 135816) by folding LVPUSHF into the shared LVPUSHT body and
\ tail-branching LVBINIPREP into the single LVBINPREP base; the shave stays inside
\ the same 4 KiB text page, so the page-rounded whole-file total is unchanged. See
\ the exact CODE-TEXT/floor rows in test/gate-size-attribution-test.f.
\ 2026-07-21 macOS 165367 -> 148855: three shared-engine landings that spark
\ re-measured on linux-arm64 only were owed a macOS re-measure - the direct-BL call
\ emitter (bc19e56e, linux -3344), the checked munmap primitive (541b691f, linux
\ +136), and the shared declaration-event transaction (8763905f, linux +44). Their
\ macOS __text delta sums byte-for-byte to linux (CODELEN 129888 -> 126724 = -3164),
\ which crosses the 16 KiB __TEXT page floor: the file drops one 16 KiB page and the
\ code signature loses four code-directory hash slots (-128), 165367 -> 148855. The
\ interleaved rigid-domains (ac5901a3) and LOAD-CORPUS rename (2afcc679) touch only
\ runtime source, not baked __text. Exact CODE-TEXT/signature/floor rows in
\ test/gate-size-attribution-test.f.
\ 2026-07-21 Linux 143552 -> 139456: retiring the vestigial EM-SNAPSHOT-REBASE-CALLS
\ snapshot scan (dot habu-retire-vestigial-snapshot-e4187b76) nets -392 bytes of engine
\ text (CODELEN 135252 -> 134860). The direct-BL landing had left the scan a no-op; the
\ shave crosses back under the 4 KiB floor the munmap primitive had pushed the file over,
\ so the whole-file total drops one page. Exact rows in test/gate-size-attribution-test.f.
\ 2026-07-31 macOS 148855 -> 148855: the snapshot-relocation stack grows engine
\ text by 1680 bytes (CODELEN 114988 -> 116668) - the address-cell table and its
\ `defer`/`is` declaration sites, the relocated region-address literals, the `xt!`
\ store-and-declare primitive, and the RBASE-VA name-bounds check. Header + code
\ is 120764 bytes, still inside the same eight 16 KiB __TEXT pages, so the pad
\ absorbs all of it and the page-rounded whole-file total is unchanged. Exact
\ CODE-TEXT/floor rows and the per-region attribution are in
\ test/gate-size-attribution-test.f.
148855 constant GB-SIZE-BASELINE-MACOS
127168 constant GB-SIZE-BASELINE-LINUX   \ fable re-measure 2026-07-21 (DGX Spark linux-arm64):
                                         \ retiring the vestigial snapshot rebase-calls scan
                                         \ (habu-retire-vestigial-snapshot) shaves CODELEN to 134860,
                                         \ dropping the whole file back under the 4 KiB floor to 139456.
                                         \ Exact rows in test/gate-size-attribution-test.f.

0 constant GB-SIZE-OK
1 constant GB-SIZE-GROWN
2 constant GB-SIZE-SHRUNK
3 constant GB-SIZE-MISSING

: GB-SIZE-BASELINE ( -- n )
   HB-TARGET-MACOS? if GB-SIZE-BASELINE-MACOS exit then
   HB-TARGET-LINUX? if GB-SIZE-BASELINE-LINUX exit then
   0 ;

: GB-SIZE-CLASS ( n n -- n ) {: sz:n base:n :}
   base 0= if GB-SIZE-MISSING exit then
   sz base > if GB-SIZE-GROWN exit then
   sz base < if GB-SIZE-SHRUNK exit then
   GB-SIZE-OK ;

: GB-SIZE-PAIR. ( n n -- ) {: sz:n base:n :}
   s" candidate " type sz .
   s" baseline " type base . ;

: GB-SIZE-GROWN-FAIL ( n n -- )
   GB-SIZE-PAIR. cr
   s" candidate size ratchet: grew past test/gate-build-size.f baseline" GE-FAIL ;

: GB-SIZE-STALE-FAIL ( n n -- )
   s" STALE-BASELINE " type
   GB-SIZE-PAIR. cr
   s" candidate size ratchet: shrank below baseline - lower the test/gate-build-size.f row in this commit" GE-FAIL ;

: GB-SIZE-MISSING-FAIL ( n n -- )
   GB-SIZE-PAIR. cr
   s" candidate size ratchet: no baseline row for this target - commit the measured size to test/gate-build-size.f" GE-FAIL ;

\ Pure class->action mapping so the wiring itself is testable: 0 = pass,
\ nonzero = the failing class. Every non-OK class must map to itself.
: GB-SIZE-ACTION ( n -- n ) {: class:n :}
   class GB-SIZE-OK = if GB-SIZE-OK exit then
   class ;

: GB-SIZE-ENFORCE ( n n -- ) {: sz:n base:n :}
   sz base GB-SIZE-CLASS GB-SIZE-ACTION
   case
      GB-SIZE-GROWN of sz base GB-SIZE-GROWN-FAIL endof
      GB-SIZE-SHRUNK of sz base GB-SIZE-STALE-FAIL endof
      GB-SIZE-MISSING of sz base GB-SIZE-MISSING-FAIL endof
   endcase ;

: GB-SIZE-CLASS-EXPECT ( n n n -- ) {: sz:n base:n want:n :}
   sz base GB-SIZE-CLASS want <> if
      s" candidate size ratchet classifier" GE-FAIL
   then ;

: GB-SIZE-ACTION-EXPECT ( n n -- ) {: class:n want:n :}
   class GB-SIZE-ACTION want <> if
      s" candidate size ratchet action wiring" GE-FAIL
   then ;

: GB-SIZE-SELF-CHECK ( -- )
   7 7 GB-SIZE-OK GB-SIZE-CLASS-EXPECT
   8 7 GB-SIZE-GROWN GB-SIZE-CLASS-EXPECT
   6 7 GB-SIZE-SHRUNK GB-SIZE-CLASS-EXPECT
   7 0 GB-SIZE-MISSING GB-SIZE-CLASS-EXPECT
   GB-SIZE-OK GB-SIZE-OK GB-SIZE-ACTION-EXPECT
   GB-SIZE-GROWN GB-SIZE-GROWN GB-SIZE-ACTION-EXPECT
   GB-SIZE-SHRUNK GB-SIZE-SHRUNK GB-SIZE-ACTION-EXPECT
   GB-SIZE-MISSING GB-SIZE-MISSING GB-SIZE-ACTION-EXPECT ;

: GB-SIZE-RATCHET ( ptr u8 n -- )
   GB-SIZE-SELF-CHECK
   FILE-SIZE GB-SIZE-BASELINE GB-SIZE-ENFORCE ;

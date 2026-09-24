---
title: Drop private signatures and symbols at capture
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.729611+03:00"
---

## Current RCA and implementation boundary

The Mac baseline identified in the Optimization parent has 16,689 real
symbols. Of 8,553 without dictionary entries, 8,502 are private. The absent
symbols carry 142,949 string bytes and 136,848 relocation bytes; latest private
effects without dictionary entries cost another 112,466 encoded header bytes.
These are retained costs, not a proven removable total.

Native build selects PAYLOAD-PERSISTENT (mode 2), bypassing the mode-1
dictionary-scoped signature export. CHECKER-CAPTURE-PREPARE persists all symbols
and effect storage; it does not reconcile them with dictionary selection.
See checker.f SYM-SNAPSHOT-PERSIST/SYM-SNAPSHOT-MARK-POINTERS, aot-capture.f
PAYLOAD-CAPTURE, and native-build-core.f LOAD-TARGET.

Retain the published dictionary interface plus explicit checker/type roots.
Twenty-one dictionary-absent symbols are referenced by primitive axioms;
constructor metadata also carries symbol IDs. Keep IDs stable initially and
trace effect, primitive, control/defer, unsafe, parsing-immediate and constructor
references before removing rows. Sealing alone does not prove metadata dead.
Of the absent private symbols, 6,611 belong to sealed packages, 1,886 to
unsealed packages and five have no package record.

Acceptance includes public checked calls, retained syntax/primitive overloads
and constructor types, rejection of unavailable private names, rollback,
restored compilation and native self-build. Measure real section deltas;
complete the native fixpoint, test/run.f and Maki smoke before closure.
Coordinate history compaction with habu-compact-checker-histories-3a1ce692.
Independent string representation work is habu-store-checker-names-70a89ffb.
Temporary reproduction: habu-symbol-retention-audit.f and habu-effect-rca.f
under ~/.cache/tmp. No implementation is claimed.

## Source reconstruction constraint

A real tier-1 capture of private `1 constant HIDDEN` and public
`KEPT ( n -- n ) HIDDEN +` retains neither the constant's name nor its body.
Nevertheless, `VERIFY:SOURCE-BUF` can check KEPT's source alone while HIDDEN's
checker symbol remains. Retiring that symbol makes the same source fail with
E-UNDEFINED; compiled KEPT still returns the expected value. Supplying the
complete source or restoring the symbol restores verification. Dictionary and
native-code roots therefore do not cover current source reconstruction.

Preserve that behavior with source-dependency roots, including constants
eliminated during lowering. Removing live private metadata without those roots
would change the reconstruction contract. No native self-build failure was
demonstrated by this probe. A narrower candidate is already-retired symbols
without registry/checkpoint roots; its savings remain unmeasured.

Preserve rooted effect histories and source-order horizons. Shared effect nodes
can outlive the header that introduced them; clearing that header's roots would
hide those nodes from UIX rebuild. Exact dictionary selection must also precede
any metadata sweep: LOAD-TARGET calls CHECKER-CAPTURE-PREPARE before compilation
finishes. Repeatable probes and observed limits are in
`~/.cache/tmp/habu-opt-names-scratch/prune-design/results.md`.

## Earlier task context

Problem: the checker user-signature store (USIGS-USER, 1.96 MB of the image as sparse runs) and the symbol tables (SYMS-BOOT, SYM-STR-BOOT, 571 KB) carry an entry for every one of the engine 15,470 words, but 8,022 are package-private and, once every captured package is sealed, no REPL source can name them, so their signatures and symbols are dead weight in every engine and every application image. Acceptance: at capture, after the seal, the signature and symbol entries of private words of sealed packages are removed (or never captured) with the tables compacted, keeping every entry the public surface, the keep-set and the checker own bookkeeping need; the checker still checks user code against every public word and refuses private names as undefined; engine-size.f reports the store and symbol bytes before and after; byte fixpoint; full gate and stripped-application suites green; Radar and Tender green on the result. Files: src/core/checker.f (capture-time compaction), src/habu/aot-capture.f, tools/engine-size.f. Verify: tools/engine-size.f; tools/native-build.f fixpoint; test/run.f; downstream suites. Depends: the seal child. Ownership: checker capture. Claim: unassigned. Parent: the ship-only-the-surface epic.
Scope note 2026-09-16 17:35 (Joel): the same rule applies to every word the seal marks internal, global or package-public, not only package-private ones; the surface list and the sealing of all other globals is the sibling dot 'Declare the surface and seal every other global as internal', and this dot strips signatures and symbols for the whole sealed-internal set.

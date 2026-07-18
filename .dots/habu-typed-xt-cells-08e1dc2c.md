---
title: "Typed xt cells: tick-store retype into xt<effect> cells"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T21:26:02.475437+02:00"
---

RE-SCOPED 2026-07-18 (orchestrator, after worker investigation): the cell capability this dot originally asked for ALREADY LANDED via habu-typed-xt-storage-ddad4af8 - CHECKER-STORAGE-INFO admits T-QUOT (src/core/checker.f:2879), TYPED-VARIABLE/TYPED-BUFFER declare persistent monomorphic code cells, a fetch recovers xt<E>, execute fit-checks E, all covered by test/xt-cell-test.f (which also pins the raw-variable launder fixture the RSEXEC flip in habu-checker-exec-of-5923c543 will turn into a reject). This dot's original premise ("CHECKER-STORAGE-INFO explicitly rejects quotation cell types") was stale RCA text.

REMAINING SCOPE (what this dot now owns): the tick-store completeness gap. `['] W HK !` into a typed xt cell FALSELY REJECTS today - the tick is erased to a plain n before the store, so it produces the same diagnostic as `42 HK !`; only the quotation form `[: W ;] HK !` works, which adds a call frame per dispatch and hides the stored word's identity. Fix as scoped by the checker-lane worker: extend BTICK-NEXT-CONSUMES-XT? (src/core/checker.f:8236) to also return true when the next token's certified signature output is `ptr xt<...>` (a typed-xt-cell store accessor), so the tick retypes to xt<effect(W)> and the existing store fit-check validates the effect. Out of scope (documented limit): the buffer form `['] W idx BUF !` puts the index between the tick and the accessor - single-token lookahead cannot catch it, quotation-store covers buffers; record that limit in docs/effects.md. Deliverables: the lookahead extension; positive fixtures (matching tick-store certifies); negatives (mismatched tick-store rejects WITH the effect-mismatch diagnostic, proven by dumping it; `42 HK !` still rejects; test/xt-effect-test.f v1-v9 and test/xt-cell-test.f stay green). Seed-affecting: byte-fixpoint + full battery. This unblocks the hook migration (step 2 in habu-checker-exec-of-5923c543) to use idiomatic `' W XV !` stores with zero added indirection.

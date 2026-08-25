---
title: Retire events with checker scope
status: open
priority: 2
issue-type: task
created-at: "2026-07-23T06:05:05.535816+02:00"
---

Problem: `VERIFY:SOURCE-BUF` opens one checker rollback scope, replays PRODUCT through the declaration coordinator, and retires type, schema, and field rows while leaving published DECL-EVENT rows behind. A surviving FIELD event then names a retired field and throws `E-PF-ID`. This parent tracks the complete atomic-lifetime correction; implementation is split into six bounded children.

Order: `habu-reserve-registry-rollback-a9d53ed5` adds fail-before-mutation reserve/readiness phases to checker rollback hooks. `habu-own-type-registry-e8f77b18` owns atomic type/schema frames. `habu-own-declaration-event-643f3573` owns event frames and byte scrubbing after field ownership lands. `habu-compose-registry-rollback-7e5742b7` installs the sole ordered composer. `habu-load-registry-rollback-d955db59` adds native/bootstrap/fixpoint composition. `habu-prove-verifier-event-f3454331` proves the real verifier and CHECK preverify lifetimes. The raw-registry lexer/package-lint chain gates source acceptance but does not own runtime behavior.

The parent closes only when all six children land and real `VERIFY:SOURCE-BUF` success and failure restore references with their targets, `SOURCE-BUF-IN-SCOPE` retains both until its outer scope closes, direct public PRODUCT persists, nested scopes retire only inner rows, every surviving event resolves, and mutations omitting save, readiness, scrub, or the frozen restore order fail. No generic callback registry, second installer, verifier cleanup, raw forwarding global, public mutation surface, TRUSTED boundary, or test-only state model is allowed.

---
title: Reserve registry rollback frames
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T07:47:11.326040+02:00"
---

Problem: CHECKER RBF-PUSH mutates core frame state before extension SAVE, so a type/schema allocation failure leaves parallel depths split. RBF-POP and RBF-FINALIZE likewise let an extension mutate before another owner rejects. Owner: CHECKER rollback extension protocol only. Add REG-EXT-RB-RESERVE-XT, REG-EXT-RB-RESTORE-READY-XT, and REG-EXT-RB-FINALIZE-READY-XT beside the existing save, restore, and finalize hooks. Sequence RBF-PUSH as core capacity preparation, extension RESERVE, core frame write/depth increment, then infallible extension SAVE. Sequence POP and FINALIZE as extension readiness checks before any extension or core mutation. Defaults are no-op. Do not add a callback registry, catch-and-repair path, TRUSTED boundary, or runtime fallback. Acceptance: injected reserve failure leaves every core and extension byte/depth unchanged; after successful reserve, SAVE has no failure path; restore-ready and finalize-ready failure leave all marks and bytes unchanged; nested scopes preserve strict LIFO order. Mutating any phase order fails the focused production checker-scope test. Files: src/core/checker.f and focused checker rollback tests. Smallest check: CHECKER-SCOPE-START/DONE/FINALIZE through the real hooks with mutation probes; typed-local and package gates.

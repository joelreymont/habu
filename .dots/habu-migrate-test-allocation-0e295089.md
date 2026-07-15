---
title: Migrate test allocation callers
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.902405+02:00"
blocks:
---

Full context: exact test direct callers of legacy MEM-ALLOC-BYTES are test/gate-build-common.f, test/gate-engine-lib.f, test/gate-pool.f, test/gate-stdlib-inline-lib.f, test/run-result-cache-test.f, and test/seal-absence.f. Fix only these test owners to construct positive typed byte lengths before MEM:ALLOC-BYTES. Acceptance: intentional negative fixtures remain intentional, pool/build/cache behavior unchanged, raw/zero accidental calls reject, fixed-string census complete. Depends on packaged MEM owner.

---
title: "Trust lint: scan maki and tools"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-12T10:12:41.102001+02:00\""
---

Problem: tools/trust-lint-core.f repo-wide discovery walks only src/ and lib/, so TRUSTED sites in maki/ and tools/ can be absent from TRUSTED.md while trust-lint remains green; target/toolchain/region owners proved this with 24 unmanifested refinements. Fix: derive audited source roots from the canonical repository source manifest or explicitly include maki/ and tools/ recursively, with no silent missing-directory fallback; keep trusted-inventory and trust-lint ownership semantics aligned. Acceptance: an unmanifested TRUSTED site under nested maki/target/ and tools/ each makes the lint fail with file:word; manifested rows pass; recursion covers nested directories; existing 0 set-check and TRUST parsing remains exact; no duplicate counting. Files: tools/trust-lint-core.f, tools/trust-lint-test.f and fixtures, source manifest only if canonical ownership requires it, TRUSTED.md only for fixture/real rows. Verify: red-first nested maki/target and tools fixtures, trust-lint, trusted-inventory strict, host-lint, filemap-lint, full test/run.f. Depends: none. Ownership: trust-lint recursive discovery and focused fixtures only; do not add target/toolchain/region trust rows or edit compiler/package/storage owners.

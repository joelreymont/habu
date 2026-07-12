---
title: "Trust lint: scan maki and tools"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T10:12:41.102001+02:00"
---

Problem: tools/trust-lint-core.f repo-wide discovery walks only src/ and lib/, so TRUSTED sites in maki/ and tools/ can be absent from TRUSTED.md while trust-lint remains green; target/toolchain/region owners proved this with 24 unmanifested refinements. Fix: derive audited source roots from the canonical repository source manifest or explicitly include maki/ and tools/ recursively, with no silent missing-directory fallback; keep trusted-inventory and trust-lint ownership semantics aligned. Acceptance: an unmanifested TRUSTED site under nested maki/target/ and tools/ each makes the lint fail with file:word; manifested rows pass; recursion covers nested directories; existing 0 set-check and TRUST parsing remains exact; no duplicate counting. Files: tools/trust-lint-core.f/test, source manifest if needed, TRUSTED.md. Verify red-first fixtures, trust-lint, trusted-inventory strict, host-lint, filemap-lint, full test/run.f.

---
title: temp-dir seed copies the prefix length, not the retry index
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T12:04:40.955504+02:00"
---

Problem: lib/fs-mutate.f:236-238 FS-MUT-MAKE-TEMP-DIR-SEED keeps the retry counter on the data stack under five pushed locals, so 'over' copies prefixu (the prefix length) instead of the retry index: every temp dir is named <prefix>-<mono-ns>-<len(prefix)> (measured 2026-08-23 by the AOT lane: hb-gate-aot-bundle-data -> -23, hb-gate-aot-preseed -> -19, habu-aot-span -> -13, identical across runs 12 hours apart), so all 64 retries build the same path and a real collision cannot be escaped - it ends in E-FS-IO. Acceptance: the retry index reaches the name (a local, not stack arithmetic under locals); a test forces a collision on the first name and asserts the second attempt differs and succeeds; the 64-retry exhaustion path asserted with a named code. Files: lib/fs-mutate.f, lib/fs-mutate-test.f. Verify: the test. Depends: none. Ownership: fs. Claim: unassigned.

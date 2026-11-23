---
title: Report hb source and tool input faults as rejections
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.600104+01:00"
---

Problem: src/habu/primitive-registry.f:42, :137, :145 check prims.f against the backend bodies while hb builds, and tools/check-verify-child-lib.f:99 (a < STDIN frame) and tools/lint/clobber-lint-core.f:58 (the linted file) are reached by input; each ends in an internal die instead of a rejection naming the file and the fix. So do the checks of hb's own source while it builds: src/habu/crash.f and crash-x64.f DIE-LINE-SITE, (text length) and crash.f FAULT-TEXT, (register), src/habu/fault-record.f CHECK-TEXT, src/habu/prims.f ETRUSTED-ONLY!, EIMMEDIATE! and EFIXED-ABI! (before any row), src/habu/data-claims.f CLAIM, tools/lint/text.f LINT-CODE-NAME+, src/core/checker.f INTRINSIC (id range) and SCRATCH{ / }SCRATCH (pairing) (census .out/census/dies-c.md, classes OTHER and REJECT). Acceptance: each is a rejection in the Rust-style form; no internal die remains there. Depends: none. Claim: unassigned.

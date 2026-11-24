---
title: Report hb source and tool input faults as rejections
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.600104+01:00"
---

Problem: tools/check-verify-child-lib.f TABLE-FAULT (a < STDIN frame) and tools/lint/clobber-lint-core.f:58 (the linted file) are reached by input, and each ends in an internal die instead of a rejection naming the file and the fix. So do these checks of hb's own source while it builds: src/habu/fault-record.f CHECK-TEXT, tools/lint/text.f LINT-CODE-NAME+, src/core/checker.f INTRINSIC (id range) (census .out/census/dies-c.md, classes OTHER and REJECT). Acceptance: each is a rejection in the Rust-style form; no internal die remains there. Depends: none. Claim: unassigned.

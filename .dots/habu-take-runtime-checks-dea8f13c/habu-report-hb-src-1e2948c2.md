---
title: Report hb source and tool input faults as rejections
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.600104+01:00"
---

Problem: src/habu/primitive-registry.f:42, :137, :145 check prims.f against the backend bodies while hb builds, and tools/check-verify-child-lib.f:99 (a < STDIN frame) and tools/lint/clobber-lint-core.f:58 (the linted file) are reached by input; each ends in an internal die instead of a rejection naming the file and the fix (census .out/census/dies-c.md, classes OTHER and REJECT). Acceptance: each is a rejection in the Rust-style form; no internal die remains there. Depends: none. Claim: unassigned.

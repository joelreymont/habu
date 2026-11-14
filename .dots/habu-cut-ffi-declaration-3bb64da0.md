---
title: Cut FFI declaration cells nothing reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T23:36:25.865575+01:00"
---

Problem: src/lib/ffi-abi.f ARG-EXT and ARG-EXT-ARG hold values only ever compared with NO-EXT, and OPEN counts clauses (1 OPEN +!) that nothing reads. Acceptance: each holds only what its readers read. Depends: none. Claim: unassigned.

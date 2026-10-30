---
title: Fix the FFI-T-SUM10 citation in x86-64-kernel-ffi.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.311763+02:00"
---

Problem: test/x86-64-kernel-ffi.f:5 cites 'src/lib/ffi-test.f FFI-T-SUM10', which does not exist; the stub is SUM10 in test/ffi-stub-child.f. Acceptance: the comment names the real word and file, or is gone. Files: test/x86-64-kernel-ffi.f. Verify: rg FFI-T-SUM10 prints nothing. Depends: the test prune landing (it may already cut the line). Ownership: that comment. Claim: unassigned.

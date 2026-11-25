---
title: Make the x86-64 harness rows take str
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T10:50:23.851343+01:00"
---

Problem: test/x86-64-boot-harness.f X64HARNESS:RECORD, (:133) still takes ^u8 n, and test/x86-64-peer-harness.f X64HARNESS:WRITE takes ^u8 n because boot-harness calls it (its callers in test/x86-64-main-entry.f and x86-64-skel-image.f split a str for it). So test/x86-64-kernel-engine.f, -policy.f, -prof.f, -scope-find.f and -token.f, which pass s" literals to RECORD,, stay marked split-str. Acceptance: RECORD, and WRITE take str; their callers (with x86-64-kernel-atomics.f and -definition.f) pass it whole; the five files are unmarked. Depends: none. Claim: unassigned.

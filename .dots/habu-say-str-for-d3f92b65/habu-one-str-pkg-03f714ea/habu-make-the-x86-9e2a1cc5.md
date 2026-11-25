---
title: Make the x86-64 harness rows take str
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:50:23.851343+01:00"
---

Problem: test/x86-64-kernel-engine.f, -policy.f, -prof.f, -scope-find.f and -token.f stay marked split-str: they pass s" literals to X64HARNESS:RECORD, (now str), and test/x86-64-peer-harness.f X64HARNESS:WRITE still takes ^u8 n, so its callers in test/x86-64-boot-harness.f, x86-64-main-entry.f and x86-64-skel-image.f split a str for it. Acceptance: WRITE takes str and its callers pass it whole; the five files are unmarked; their suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-x86h.

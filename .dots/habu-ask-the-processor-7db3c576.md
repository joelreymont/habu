---
title: Ask the processor for SHA-256 once on arm64
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T21:07:06.743764+01:00"
---

Problem: src/habu/habu1.f SHA2-PROBE walks envp and the auxiliary vector at every sha256-blocks call, and SHA256-K repeats src/core/sha256.f's KK. Acceptance: arm64 asks once, as x86's kernel does, the constants have one table, and a warm Maki build is no slower. Depends: habu-compress-sha-256-92e128a7. Claim: unassigned.

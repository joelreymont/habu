---
title: "Drop the verifier's JIT tick order if no product path reaches it"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T21:36:46.236768+02:00"
---

Problem: with T0 as arm64 tier 0, the verifier child's tick-order@ (habu1.f BTICKORDER) answers 0 (checking first), and x86-64's kernel-x64.f always answers 0, so src/habu/verify-source.f TICK-GATE (the JIT order: refuse a trusted-only tick at its token) is reached only by unit tests that set the order by hand. test/checker-verify-order.f IMMEDIATE-CASE runs '0 set-tier tick-order@', which now answers 0 under T0, so it may cover something other than what it was written for. Source: red cv lane report (vxmotwvu). Acceptance: either a product path that reaches TICK-GATE is named, or TICK-GATE, the order cell and their tests go; IMMEDIATE-CASE asserts what it means under T0. Files: src/habu/verify-source.f, src/habu/habu1.f, test/checker-verify-order.f. Verify: check-verify row and checker-verify-order on the candidate. Depends: the T0 release landing. Ownership: those files. Claim: unassigned.

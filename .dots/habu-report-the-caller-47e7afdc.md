---
title: Report the caller when x86 package-scope! dies
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T00:19:10.997314+01:00"
---

Problem: arm64 package-scope! dies through REJECT-AT: CALLER-LR then DIE-LINE, and reports lr; x86-64 exits through SEAL-TRAP with no caller in its die line. Acceptance: both targets' package-scope! die lines name the caller the same way; the engine tests pass on both. Depends: none. Claim: unassigned.

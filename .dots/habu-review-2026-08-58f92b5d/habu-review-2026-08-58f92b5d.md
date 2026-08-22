---
title: "Review 2026-08-22: engine and native chain"
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.785659+02:00"
---

Problem: the native chain + judge audit found a miscompile class (RECURSE in quotations), a host-bound judge gate, unexercised verifier refusals, five copies of the opcode tables and Darwin ABI assumptions on the Linux host. Acceptance: every child closed or refuted. Files: src/compiler/, src/habu/, tools/judge*. Verify: per child. Depends: none. Ownership: native chain. Claim: unassigned.

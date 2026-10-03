---
title: Give each backend a session and an owned emission
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.819643+03:00"
---

Problem: NCOMP, NEMIT and NSHADOW keep compilation state in module variables and single-row buffers (compiler.f, emission.f:40-52, shadow.f:15-24); a nested or failed second-target compilation can leave parent state wrong (PA-r2 §5, §11.1, §12, P2). Acceptance: BackendSession and an ExclusiveSession lease; a sealed emission owns its bytes and rows (copy first, transfer later); BACKEND-ROWS replaced by manifest-sized storage; T07-T09 pass, with A64->x86->A64 in one process and a mid-x86 failure leaving the parent valid. Files: src/compiler/native/{compiler,backend,emission,shadow}.f, src/compiler/session/ (new), src/compiler/target.f (capacity). Verify: full gate; test/compiler/session.f (new, registered). Depends: habu-resolve-build-targets-ae8e65c1. Ownership: src/compiler/native/*.f (coordinate with the unmerged Intel pin 7554c01d edits to compiler.f/backend.f). Lane: dave. Claim: unassigned.

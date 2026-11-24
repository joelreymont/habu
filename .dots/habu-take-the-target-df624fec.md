---
title: Take the target predicates by identity in the verifier
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:58:14.273655+02:00"
---

Problem: TARGET-GUARD (src/habu/verify-source.f:1062, on master) takes HB-TARGET-LINUX?, HB-TARGET-MACOS?, HB-TARGET-LINUX-X86-64?, HB-TARGET-LINUX-KERNEL? and HB-TARGET-WASM? by spelling, so a program's own word of that name makes --verify-only and --load disagree. Found by the host-loads re-review (2026-11-10), by reading. Acceptance: a program that defines its own HB-TARGET-LINUX? by any of those routes gets the same answer from --verify-only and --load. Files: src/habu/verify-source.f. Verify: one such program per route through both commands. Depends: the design of how the verifier knows an engine word decides the mechanism. Ownership: carl. Claim: unassigned.

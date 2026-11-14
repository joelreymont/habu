---
title: Take the target predicates by identity in the verifier
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:58:14.273655+02:00"
---

Problem: TARGET-GUARD (src/habu/verify-source.f:917, on master) takes HB-TARGET-LINUX?, HB-TARGET-MACOS?, HB-TARGET-LINUX-X86-64?, HB-TARGET-LINUX-KERNEL? and HB-TARGET-KNOWN? by spelling, so a program's own word of that name makes --verify-only and --load disagree. The lint that reserves the names (tools/reserved-name-lint-core.f:115-126, run by CHK-RUN-RESERVED-NAMES tools/check-core.f:1568-1577) runs only in tools/check.f's full mode on the subject file, never in --verify-only, the LSP or --load, and misses names after TYPED-VARIABLE, PTR-VARIABLE, TYPED-BUFFER, DYNAMIC-BUFFER, CWORK-VARIABLE, CWORK-BUFFER, PTR-U8-TABLE, 2constant, CAST:, a program's own defining word and evaluate text. Found by the host-loads re-review (2026-11-10), by reading. Acceptance: a program that defines its own HB-TARGET-LINUX? by any of those routes gets the same answer from --verify-only and --load. Files: src/habu/verify-source.f, tools/reserved-name-lint-core.f. Verify: one such program per route through both commands. Depends: the design of how the verifier knows an engine word decides the mechanism. Ownership: carl. Claim: unassigned.

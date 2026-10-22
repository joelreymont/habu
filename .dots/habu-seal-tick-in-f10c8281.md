---
title: Seal tick in tier-1 bodies
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:19:23.333260+03:00"
---

Problem: in a tier-1 body, ' (tick, a parsing keyword) passes its operand through no package-seal check in either reader: the Habu loop (src/habu/definers.f COMPILING?, x86's only loop) or ARM's native tier-1 reader (habu2.f EM-COMPILE via LBCAP). Tier-0 behaviour for ' inside a body is also unchecked (the btickseal review saw tier 0 refuse char and ' in a body with E-UNDEFINED). Found while sealing ['] (stack commit tslownok; ARM native twin habu-seal-bracket-tick-c5a8c425). Acceptance: first measure what ARM tier 0 does with ' SEALEDPKG:word in a body and at top level; then make both tier-1 readers match it (rc 84 through the seal guard if tier 0 refuses). Files: src/habu/definers.f, src/habu/habu2.f or src/compiler/native/elaborate.f. Verify: probes beside test/seal-package.f at 0 and 1 set-tier. Depends: none. Ownership: unassigned. Claim: unassigned.

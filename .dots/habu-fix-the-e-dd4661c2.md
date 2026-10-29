---
title: Fix the E-X64SEL-TRAP descriptions
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.541290+02:00"
---

Problem: src/compiler/native/select-x64.f:66 and src/lib/errors.f (E-X64SEL-TRAP, -8764) say the code also covers a trap whose routine is not in the target dictionary; select-x64.f throws it only at DSTACK? 0= and TRAP-CK. Acceptance: both comments state what the code does. Files: src/compiler/native/select-x64.f, src/lib/errors.f. Verify: rg -n 'E-X64SEL-TRAP' src; read both sites against the throw sites. Depends: none. Ownership: the two comment lines. Claim: unassigned.

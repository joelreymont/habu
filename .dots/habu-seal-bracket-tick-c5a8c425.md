---
title: Seal bracket-tick in the ARM native tier-1 reader
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:53.141965+03:00"
---

Problem: at 1 set-tier the ARM native body reader (habu2.f EM-COMPILE via LBCAP) captures the operand of ['] without the package seal, so ['] RESERVED:tail compiles (rc 0/7) where tier 0 C-BTICK -> C-QUALIFY-SEAL-GUARD refuses rc 84 (test/seal-package.f cases 86/88 fail at tier 1 with 70). The Habu reader got the guard in stack commit tslownok 'Seal bracket-tick at tier 1'. Choice to design: a native 'next token belongs to [']' cell in EM-COMPILE calling C-QUALIFY-SEAL-GUARD with its own TOK* rollback mark, vs one spelling-taking guard at elaboration (elaborate.f DO-TICK) covering both readers but firing at ';'. Also unchecked: tier-1 ' (tick) has no seal check in either reader. Acceptance: seal-package.f cases 86/88 rc 84 at 1 set-tier on ARM. Files: src/habu/habu2.f or src/compiler/native/elaborate.f. Verify: seal-package.f with 1 set-tier prepended. Depends: none. Ownership: unassigned. Claim: unassigned.

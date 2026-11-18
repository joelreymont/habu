---
title: Charge a quotation for the quotations it runs
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T22:54:19.951048+01:00"
closed-at: "2026-11-18T09:53:06.029935+01:00"
---

Problem: a quotation that runs a quotation it is given does what that one does, but nothing charges it: defer TDECL-TXN-XT (src/core/sumtype.f:1422) states ( [ -- renders parses nominal ] -- renders nominal ), and [: RUN ;] (src/core/generated-declaration-protection.f:59, RUN ( [ -- ] -- ) runs its input) is accepted into it, so a caller of the defer is charged renders nominal though the run parses. SOURCE-UNIT:LOAD's inner [ n n -- ] quotation is the same. Acceptance: a quotation kept in a slot is charged for the quotations it may run, and the program above is rejected or the defer states parses; a test fails first. Depends: none. Claim: unassigned.

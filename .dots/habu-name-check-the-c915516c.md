---
title: Name-check the inline row key
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.428518+02:00"
---

Destruction review of NINL, low. CALLEE-COPY? (elaborate.f:1516-1523) keys the splice on the caller-stated address and cross-checks only arity — an address+arity coincidence splices the wrong routine's body. The row already stores the callee's spelling (R-SPELL): add a name cross-check so a numeric coincidence becomes a structural refusal. Related latent hole: rows store spellings and INL-SYM re-resolves them in the CALLER's word model; a caller declaring the same spelling as fixed data at a different address would splice its own address silently. Guarded today only by CALLEES-NONE-CK (migrate.f:562-567), which dot habu-resolve-a-data-a1c8067f proposes to remove — that dot must not land before this check exists.

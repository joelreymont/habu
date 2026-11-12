---
title: A TRUST of a checked word weakens its proven facts
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T11:58:20.002324+02:00"
---

Problem: in the cross-build the build's checker checks src/core/pointer-storage.f, so PTR-VARIABLE carries its created effect; src/core/cell-effects.f:36 's" PTR-VARIABLE" s" --" TRUST' then re-records it with no created effect (src/core/checker.f:17101 STORE writes creates 0 for a record that is not external), and PPRIM: rows likewise drop the throws INTERN TEXT/FIND/SPACE-STRING were proven to have. The rows exist for hb's boot from source, where those words load before the checker. Measured in the cross-build: PTR-VARIABLE's NORET-CREATES@ was 46137, then 0 once cell-effects.f's row ran. Acceptance: a TRUST, PRIM: or PPRIM: row about a word whose effect the checker proved neither weakens it nor needs a boot-only split; the rule is one for both paths. Asking Joel is required if it changes what TRUST accepts (a public word's behavior). Depends: none. Claim: unassigned.

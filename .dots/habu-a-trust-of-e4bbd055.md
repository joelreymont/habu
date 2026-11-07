---
title: A TRUST of a checked word weakens its proven facts
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T11:58:20.002324+02:00"
---

Problem: in the cross-build the build's checker checks src/core/pointer-storage.f, so PTR-VARIABLE carries its created effect; src/core/cell-effects.f's 's" PTR-VARIABLE" s" --" TRUST' then re-records it with no created effect (STORE appends creates 0), and PPRIM: rows likewise drop the throws INTERN TEXT/FIND/SPACE-STRING were proven to have. The rows exist for hb's boot from source, where those words load before the checker. Measured: PTR-VARIABLE's NORET-CREATES@ 46137 after census row 2, 0 after row 24. Acceptance: a TRUST, PRIM: or PPRIM: row about a word whose effect the checker proved neither weakens it nor needs a boot-only split; the rule is one for both paths. Asking Joel is required if it changes what TRUST accepts (a public word's behavior). Files: src/core/checker.f (TRUST, USIG-ADD-AS, STORE), src/core/cell-effects.f. Verify: census row 24 keeps PTR-VARIABLE's creates; hb --load of a program that trusts a checked word. Depends: none. Ownership: src/core/checker.f, src/core/cell-effects.f. Claim: unassigned.

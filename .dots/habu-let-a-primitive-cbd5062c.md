---
title: Let a primitive row record that its word throws
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:31:45.957341+02:00"
---

Problem: a word that loads before the checker gets its effect from a src/core/cell-effects.f row (PRIM:/PPRIM:/TRUST), and a row records a stack effect only; only rule tables (checker.f NORET-AXIOMS and the tables near :21252/:21618) set CTL-THROW. So the checker treats a call to a throwing pre-checker word as unable to throw: src/core/util.f PATHZ/PATH0 (E-PATH-RANGE, E-PATH-NUL; noted above PATHZ) and, once C2c2a lands, src/habu/intern-table.f INTERN:SPACE-STRING (E-INTERN-REWIND; noted at its cell-effects.f row). No result differs today: every checked caller throws on its own path. A checked caller that relied only on such a word's throw would get a facts text without 'throws' and a caller's catch/intact-mask check would be unsound. Found by the C2c2a review (2026-11-07). Acceptance: a row can state that its word throws (with intact masks), the PATHZ/PATH0 and SPACE-STRING rows say so, and a rejected program shows the throw reaching a caller (a checked word whose only throw comes from such a call carries 'throws' in its facts). Files: src/core/checker.f (row parser), src/core/cell-effects.f, src/core/util.f comment. Verify: a type-rule test through the real load path; tests checker-soundness engine. Depends: habu-c2c2a-move-the-34ad64fd landed. Ownership: src/core/cell-effects.f rows. Claim: unassigned.

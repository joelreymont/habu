---
title: Route a code-valued comma to a declaring store
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T17:20:32.573633+03:00"
---

Problem: create T ' W , records no address-cell declaration because , is an untyped store (src/habu/layout.f SNAP-RELOC XTCELL band is fed only by defer, is and xt!, and src/core/quotation-storage.f STORE picks xt! for a checker-proven quotation store). A stripped image that holds such a cell in its captured DATA is refused by hb-build by name (the refusal after habu-let-a-stripped-0a064bf5 names the declaring forms), so the CASE-TICK shape from test/aot-strip cannot become a working image even though the checker knows the stored value is an xt. Acceptance: when the elaborator sees , applied to a checker-proven xt (or a quotation) it routes the store to a declaring form (an xt, sibling of xt!) so the cell joins the declared address-cell table; the stripped-image relocation of 0a064bf5 then rebases it like a defer cell; regression: a stripped image built from create T ' W , executes W through T @ execute, and a , of a plain n still declares nothing. Rejected program: a , of an xt inside an unchecked TRUST body stays undeclared and stays refused by hb-build. Files: src/core/checker.f or the elaborator module that types , (find the STORE hook quotation-storage.f uses), src/habu/layout.f, src/habu/habu2.f, test/aot-strip*.f. Verify: focused strip suites; tools/native-build.f fixpoint; test/run.f. Depends: habu-let-a-stripped-0a064bf5. Ownership: checker store elaboration and strip. Claim: unassigned.

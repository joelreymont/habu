---
title: "Compare a pointee's nested quotation inputs both ways"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T10:21:52.626079+01:00"
closed-at: "2026-11-18T11:24:41.894921+01:00"
---

Problem: src/core/checker.f ROLE-FLIP (:4801-4802) turns a pointee or store-slot pair's role 3 into the join role 0, so a quotation input nested under a pointee is joined (QUOT-PAIR :4829, :4838-4840) instead of compared both ways. A word `( ^[ [ -- ] -- ] -- )` that stores a keeper of a `[ -- ]` cell through the pointer into a `[ [ -- renders ] -- ]` cell is admitted; the keeper is then handed a rendering body, so `--load` runs it while `--verify-only` rejects a later use of the word it defined (E-UNDEFINED). The direct store is rejected. Acceptance: that program is rejected where the pointer is handed over, under `--load` and `--verify-only` alike, with a test beside test/xt-effect-test.f's pointee case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nA.

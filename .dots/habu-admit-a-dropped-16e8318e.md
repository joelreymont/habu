---
title: Test the admitted drop of an option after catch
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:25:52.611709+02:00"
---

Problem: the checker now admits a stale non-linear value that the docs admit, with no test that holds it. With `require src/lib/adt/option.f`, `: P ( option<n> -- option<n> ) drop 0 dup 0= if -1 throw then OPTION:SOME ;` and `: X ( -- option<n> ) OPTION:NONE [: P ;] catch {: code:n :} code 0<> if drop code throw then ;` load, as does the `['] P catch` form; both used to exit 70 at X's `drop` (E-REJECTED). docs/forth.md 'A caught literal leaves possibly overwritten inputs stale' (:2590) and 'Linear and multi-cell values respect the stale boundary' (:2616) say a stale non-linear value may be dropped; option<n> is not linear. src/lib/net/bearer.f:675-676 still says UNANSWERED exists because a bare OPTION:NONE is E-REJECTED at the 2drop (rc 70). The no-catch case (`: Y ( -- ) OPTION:NONE drop ;`, still refused) is habu-a-dropped-bare-6d7bb8a7.
Acceptance: test/catch-stale-suite.f holds X and the `['] P catch` form as admitted cases, with a linear value in the same shape still refused; UNANSWERED's comment stops citing the refusal.
Depends: none. Claim: unassigned.

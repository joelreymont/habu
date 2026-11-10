---
title: Test the admitted drop of an option after catch
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:25:52.611709+02:00"
---

Problem: the checker used to refuse a stale non-linear value that the docs admit, and now admits it with no test that holds it. With `require src/lib/adt/option.f`, `: P ( option<n> -- option<n> ) drop 0 dup 0= if -1 throw then OPTION:SOME ;` and `: X ( -- option<n> ) OPTION:NONE [: P ;] catch {: code:n :} code 0<> if drop code throw then ;` exited 70 at X's `drop` (E-REJECTED, repair_class unknown_rejection, inferred effect `-- stale<option<n>>`), as did the `['] P catch` form; both now load (on the audit's engine, some commits behind master). docs/forth.md 'A caught literal leaves possibly overwritten inputs stale' (:2648) and 'Linear and multi-cell values respect the stale boundary' (:2672) say a stale non-linear value may be dropped; option<n> is not linear. src/lib/net/bearer.f:689-690 still says UNANSWERED exists because a bare OPTION:NONE is E-REJECTED at the 2drop (rc 70); the other workarounds this dot named went with test/compiler/native-catch.f. The no-catch case (`: Y ( -- ) OPTION:NONE drop ;`, still refused) is habu-a-dropped-bare-6d7bb8a7.
Acceptance: test/catch-stale-suite.f holds X and the `['] P catch` form as admitted cases, with a linear value in the same shape still refused; UNANSWERED's comment stops citing the refusal.
Files: test/catch-stale-suite.f, src/lib/net/bearer.f.
Verify: bin/hb --load test/catch-stale-suite.f; bin/hb --load src/lib/net/bearer-test.f.
Depends: none. Ownership: unassigned. Claim: unassigned.

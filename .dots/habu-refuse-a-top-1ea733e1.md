---
title: Refuse a top-level addrmap-set with a diagnostic
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T04:35:50.336627+02:00"
---

Problem: at top level, `0 addrmap-set` (a private row plus a global trusted-only row) runs and the process exits 83 ENGINE-ERROR:SEAL-VIOLATION with no diagnostic; measured on the TOP-JUDGE engine (carl-topjudge e3, xuzvvpvz) and on its parent-commit engine alike, so it predates the rowless-step rule. Rule: a top-level step the checker cannot admit is refused at the step with a diagnostic naming the word and the reason; the seal never answers for the checker. Acceptance: the step is refused before it runs, with the word and reason on stderr and the checker's exit code; a row in test/outer-interpret.f fails on today's engine and passes with the fix; no special case for addrmap-set. Files: src/core/checker.f (TOP-JUDGE, TOP-ANSWER, the trusted-only row admission), test/outer-interpret.f. Verify: the row; the three-line probe. Depends: habu-refuse-a-top-57851ac2. Ownership: src/core/checker.f top-level judging. Claim: unassigned.

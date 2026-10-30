---
title: Refuse a qualified reach to a global engine word
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T05:51:46.339438+02:00"
---

Problem: the engine's find resolves a qualified token whose package does not hold the name to the global word of that name: `CVT-OFP:evaluate` finds the global `evaluate` (seen 2026-10-30 in tools/check-verify-test-lib.f TOP-PACKAGE-TAIL on the installed hb, which ran it and printed 7; the TOP-JUDGE lane 57851ac2 now refuses it at top level because a qualified token carries no identity and the global evaluate has no row, so the case was changed to CVT-OFP:evaluate-closed). A qualifier that names a package the word is not in should not resolve: it hides a wrong package name and lets a program reach an engine word through any package. Acceptance: the engine's find answers a qualified name only from the named package (and whatever the package explicitly exports or inherits by the documented rule); a qualified name of a global word is refused as undefined with the qualifier named, at top level and in a body; a regression row in test/outer-interpret.f and the card §1 rule. First a census: tools/check.f over tools/ and test/ with the refusal in place, every qualified reach to a global word listed and fixed at its site. Files: the engine's find (src/habu/habu1.f or habu2.f qualified lookup; src/core/checker.f CHECKER-RESOLVE:WALK, BIND-TOK), docs/forth-card.md §1, test/outer-interpret.f. Verify: the regression row; tools/check.f census; bin/hb --load test/run.f (engine-baked lookup). Depends: habu-refuse-a-top-57851ac2 (lands first). Ownership: carl. Claim: unassigned.

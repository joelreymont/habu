---
title: Reap the pool root a suite-less pool user makes
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T17:25:03.838695+03:00"
---

Problem: a pool user with no GT-START (test/nf-path-test.f run standalone is one) gets GT-POOL-FALLBACK-ROOT$ = <HB_TMP|TMPDIR|/tmp>/hb-pool-<mono-ns> and nothing removes it: only GT-POOL-TMP$ has a REMOVE-TREE, so each standalone run leaves a root with four small capture logs (three sat under the test-pools lane's HB_TMP after two standalone runs). Under a suite the root lands in the slot directory and is reaped. Acceptance: the fallback root goes at the end of the drain or through a GT-CLEANUP-style call the suite-less user makes, after the red report (it rebuilds capture paths under that root); test/nf-path-test.f run standalone under HB_TMP=$X leaves no hb-pool-* in $X. Files: test/gate-pool.f. Verify: the standalone run and ls. Ownership: hazel. Claim: unassigned.

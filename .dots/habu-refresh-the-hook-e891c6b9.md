---
title: Refresh the HOOK-THROWN comment after 90d909
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:19:23.358895+03:00"
---

Problem: test/compiler/native-eval.f:348 says HOOK-THROWN runs at tier 0 alone because 'tier 1 refuses W-FORGET itself for that ambiguity before its hook runs'. The Intel stack commit 90d909695b18 'Read a definition's prior name quietly' (src/compiler/native/compiler.f READ-PRIOR) removes that early 7141 refusal: at the stack tip ARM 1 set-tier runs the hook first, prints 77 and refuses E-USING-SHADOW-GLOBAL at T-FORGET, same as x86. On master the comment is still true. Acceptance: once 90d909 is on master, rewrite the comment to the measured fact; keep or drop the tier-0-only restriction according to that fact (master test assertions win). Files: test/compiler/native-eval.f. Verify: bin/hb --load test/compiler/native-eval.f rc 0 on ARM. Depends: habu-rebase-gate-and-62c0a60f. Ownership: unassigned. Claim: unassigned.

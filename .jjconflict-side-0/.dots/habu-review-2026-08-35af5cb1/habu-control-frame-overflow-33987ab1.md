---
title: control-frame overflow is uncheckable and fail-open
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.831338+02:00"
---

Problem: src/core/checker.f:9933-9940 CF-PUSH and 10050-10052 CF-RECURSE set UNCK (uncheckable) past 31 frames instead of rejecting (docs/forth.md:1225-1228: malformed control syntax is a rejection). Under --all-errors the hook publishes every verdict (check-hook.f:34) and CHECK counts only verdict 0 (12812-12813), so an uncheckable definition nobody calls leaves MULTI-ERR-N at 0 and the driver exits 0. The cap is three unnamed literals (32/31/30). Acceptance: overflow is CF-FAIL; multi-error mode counts verdict 1; the cap is one named constant; fixtures: a 33-deep nest is refused, and under --all-errors the exit is nonzero. Files: src/core/checker.f, src/core/check-hook.f. Verify: tools/check.f --all-errors on the fixture exits nonzero. Depends: none. Ownership: checker control flow. Claim: unassigned.

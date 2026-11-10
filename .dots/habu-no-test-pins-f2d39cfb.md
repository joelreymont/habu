---
title: No test pins the rows printed for a mismatch at then
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:14.988075+02:00"
---

Problem: no test asserts the `expected:` / `actual:` prose the checker prints when an `if` arm changes the stack's depth and the refusal lands at `then`; the control-flow reasons worker searched for one to extend and found none. A change to SUNI-JOIN's row recording would pass every test. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: one prose case in test/load-reject-diag-test.f, beside the control-flow cases, through the real load path, asserting both rows for an arm that pushes one extra cell; it fails when the rows are swapped or dropped. Files: test/load-reject-diag-test.f. Verify: that file on bin/hb, and the case failing against a copy with the rows swapped. Depends: habu-checker-refusals-that-3152182a landed (same file). Ownership: the checker's refusal tests. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-no-test-pins-f2d39cfb

Defect found while writing the test, fixed in the same commit: a depth mismatch at then for an if with no else printed its rows swapped and gave no reason; the commit adds the reasons 'the if arm changes the stack's depth' and 'the if and else arms leave different stack depths'.

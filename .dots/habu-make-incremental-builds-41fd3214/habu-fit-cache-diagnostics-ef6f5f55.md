---
title: Fit cache diagnostics in captured test output
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T05:44:29.564378+02:00"
---

Problem: test/prefix-provided-unfilled.f run with HABU_NATIVE_CACHE_DIAG=1 on the outer process dies uncaught -2504 (E-PROC-TRUNCATED): the child build's diag output overflows the test's IO-CAP $8000 capture. Acceptance: the diagnostics reach the reader without losing the test result (capture to the tree's build.out/err files rather than a fixed buffer, or the harness's file capture). Files: test/prefix-provided-unfilled.f, the PROC-CWD capture used. Verify: the test with diag set, rc 0. Depends: none. Claim: unassigned.

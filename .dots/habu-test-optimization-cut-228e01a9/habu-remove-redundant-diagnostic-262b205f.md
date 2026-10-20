---
title: Remove redundant diagnostic checker wrappers
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T14:27:03.095086+03:00"
closed-at: "2026-10-20T15:09:13.112510+03:00"
close-reason: Independent Astra review clear; combined native gate685/685 exit0, diag43.795s, checker CLI215.169s. All45 diagnostic groups,224 checker cases and raw-load references retained. Whole suite17m43.33 real,4589.60user394.82sys under independent host load; no five-minute claim.
---

Remove 305 diagnostic-position outer check.f launches and 29 redundant clause/using/shadow wrapper launches through the public CHECK API and existing descriptor capture. Preserve mode-specific packets, verdicts, paths, locations and real verifier/run children; all 45 diagnostic groups, 224 checker cases and raw reference loads remain. Regenerate clause source for each mode because CHECK reuses SB$, invalidating the builder's borrowed source pointer. Ownership: test/diag-position-test.f and these helpers in tools/check-test-lib.f; agent reduce_checker_setup, workspace carl-suite-diag. Verified exit 0: original diagnostic row real/user/sys 298.63/196.52/33.18 s, revised 45.23/36.12/6.30 s; corrected checker CLI/core row 184.64/151.62/27.74 s. Conditions: matching unchanged engine, closed stdin, ulimit 512, same external HB_TMP and cache configuration. A concurrent full gate and varying host load prevent an additive gate-savings claim. Independent review, combined gate validation and landing remain integrator work; keep this claim active until accepted.

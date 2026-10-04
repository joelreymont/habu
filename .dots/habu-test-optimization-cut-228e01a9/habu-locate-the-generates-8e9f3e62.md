---
title: "Locate the generates: and FUNCTION: reader stops"
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T04:44:31.445124+03:00"
---

Problem (batch 4g run 2): master added seven `74 die` sites to src/habu/verify-source.f's `generates:` and `FUNCTION:` readers after lexloc's base, so those stops still end the pre-verifier with an unlocated prose die and no record, unlike the statement stops lane 553 (44279554 part B) made located throws. Fix: each becomes a STATEMENT-STOP or a reader throw with an existing E-VS-* code where one fits, else a new code, rendered as one located record in every check.f mode. Acceptance: one source per site seen dying first (rc 69, did not complete), then one located record, rc 70, in default, --json-errors, --all-errors and --verify-only; error-code-lint 0 findings. After: 44279554 part B on master.

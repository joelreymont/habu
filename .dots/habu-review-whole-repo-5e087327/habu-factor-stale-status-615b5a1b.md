---
title: Factor stale status scanner
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-25T12:19:43.559768+02:00\""
closed-at: "2026-06-25T21:44:08.897680+02:00"
close-reason: Factored SS-COUNT-LINE? into checked cursor, digit, ratio, whitespace, keyword, and candidate helpers; added stale-status fixture cases for uncheckable/uppercase keywords, short counts, embedded alnums, and partial ratios. Validated focused stale-status test, direct stale-status lint, test/gate-stdlib.f, and full native gate.
---

Finding F16. Evidence: docs/factorization-review.md:44; tools/stale-status-lint.f:315. Root cause: SS-COUNT-LINE? mixes digit runs, ratio parsing, whitespace skipping, and keyword checks. Fix: split into SS-SCAN-DIGITS, SS-RATIO-COUNT?, SS-COUNT-KEYWORD?, and SS-SKIP-WS with focused tests. Why: same-type scanner indexes should not be juggled through one large word. Validate with stale-status-lint-test and full native gate.

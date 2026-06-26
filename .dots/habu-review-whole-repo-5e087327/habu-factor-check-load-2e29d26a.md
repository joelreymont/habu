---
title: Factor check load builders
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.548374+02:00\\\"\""
closed-at: "2026-06-25T20:49:43.907241+02:00"
close-reason: closed by F13 all-errors core/wrapper split; tools/check-all-errors-test.f, tools/check-test.f, test/gate-stdlib.f, affected benchmark helper fixtures, and full native gate passed
---

Finding F13. Evidence: docs/factorization-review.md:41; tools/check.f:318. Root cause: CHK-ARGV-* words rebuild near-identical --load lists and expose static scanner phases as child CLI recipes. Fix: introduce checked load-group/command builder words and split static linter cores from CLI wrappers so only true boundaries spawn. Why: repeated argv recipes caused slow and hard-to-review gate behavior. Validate with tools/check-test.f, check boundary fixtures, and full native gate.

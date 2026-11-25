---
title: "Check: a left token moves the pin of its body"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:23:50.518326+01:00"
---

Problem: REFUSAL-STEP's CREFUSE branch (src/core/checker.f) moves the pin of a body already left to the run: `[: a ;] RUN P:X:Y` is reported as leaving the definition from P:X:Y with a cause about a quotation, and `MADE P:X:Y` is mispinned the same way on the installed hb. Acceptance: the pin and the cause name the same token, with a test in test/diag-position-test.f. Depends: none. Claim: unassigned.

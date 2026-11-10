---
title: "A test label names only a case's first assertion"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.098104+02:00"
---

Problem: src/lib/test/assert.f:70-104 - T-ASSERT, T=, T<>, TTRUE and TFALSE each end with T-LABEL-CLEAR, so a failure after a case's first assertion prints no 'case:' line and TREC-FAIL records an empty label. Reported by the log-test and FIELD= workers. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: T-LABEL names every assertion until the next T-LABEL or T-LABEL-CLEAR; a failing second assertion prints its case; a test that relied on the clearing sets or clears its label itself. Files: src/lib/test/assert.f and the tests whose output changes. Verify: a case with two assertions whose second fails prints 'case:'; the suite. Depends: none. Ownership: the test library. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-test-label-66afe7c3.

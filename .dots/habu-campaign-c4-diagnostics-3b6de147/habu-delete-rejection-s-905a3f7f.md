---
title: "Delete REJECTION's dead and over-public words"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.454669+01:00"
closed-at: "2026-11-16T04:07:36.428609+01:00"
---

Problem: ORIGIN-LINE/COLUMN/BYTE, ORIGIN! and its save and restore have no reader that changes output (callers check-all-errors-core.f:295, check-verify-child-lib.f:454, diag-origin-core.f:306, diag-position-test.f:1706-1710); END-TEXT's NO-PART check cannot hold; LABELS?, LOAD-FILE$, JSON-KEY, JSON-STRING and FLUSH are public with no caller outside their package. Acceptance: the dead words and the check are gone; the five words are private. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

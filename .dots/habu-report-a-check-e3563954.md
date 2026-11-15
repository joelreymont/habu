---
title: "Report a check's first rejection, not a later one"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:43:40.719696+01:00"
---

Problem: a body calling an unmodelled immediate whose next token is undefined (/tmp/carl-payload/g4.f) is E-UNMODELED-IMMEDIATE under --load but E-UNDEFINED on the next token under tools/check.f: CALL-STEP (src/core/check-walk.f:411) records REJECT-IMMEDIATE and keeps walking, and the later throw is reported. Acceptance: tools/check.f reports the same first rejection as the load. Depends: none. Claim: unassigned.

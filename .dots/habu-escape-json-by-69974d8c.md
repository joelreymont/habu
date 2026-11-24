---
title: Escape JSON by one rule
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.526393+01:00"
---

Problem: three escapers write different bytes for one JSON string: REJECTION's JSON-CHAR and crash.f LFAULT-PUT (uppercase hex, \b and \f as \u00XX), the JSON writer (src/lib/json/write.f:286-310: lowercase hex, \b and \f by letter) and tools/check-verify-child-lib.f ESCAPE$ (:205). Acceptance: one rule, held by one word the others call. Depends: none. Claim: unassigned.

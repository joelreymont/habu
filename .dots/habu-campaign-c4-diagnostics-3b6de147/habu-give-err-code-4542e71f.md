---
title: "Give ERROR:CODE rejections a full record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T13:28:15.869094+01:00"
closed-at: "2026-11-17T16:14:34.480700+01:00"
---

Problem: src/lib/error.f CHECK (:480-500) rejects a bad ERROR:CODE declaration with one SAY line and REFUSE: no place, no source line, no expected and found, no fix, unlike every REJECTION record. Acceptance: each CHECK rejection prints a REJECTION record at the declaration's token with a help line, rc 70; a test pins one. Depends: habu-name-each-exit-3fc49d37 (deletes CHECK's 0..255 branch, error.f:505). Claim: agent=worker workspace=.jj-ws.noindex/jerry-errrec.

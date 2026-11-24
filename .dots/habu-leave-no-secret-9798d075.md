---
title: "Leave no secret answer's bytes in REASON's texts"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T22:24:44.384737+01:00"
---

Problem: the JSON reader quotes up to REASON:QUOTE-MOST bytes of a malformed input into the task's REASON text (src/lib/json/read.f:345-366 FOUND-STRING, PUT-FOUND), and nothing wipes REASON's vectors (src/lib/reason.f). A malformed 200 answer to SECRET:AZURE-BIND's vault read, or to BEARER:IDENTITY-TOKEN's metadata ask, leaves bytes of the secret or token in memory after the store wiped its own staging. Acceptance: after a malformed secret or token answer, no byte of it remains in the task's REASON texts; a test reads them after such a read. Depends: none. Claim: unassigned.

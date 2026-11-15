---
title: "Place a VALUE-RECORD's bad type at the type"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.696848+01:00"
closed-at: "2026-11-16T04:07:35.006232+01:00"
---

Problem: VALUE-RECORD with a bad type is placed at the field name, not the type token; TYPED-VARIABLE prints a redundant `= word:` note. Acceptance: the record stands at the type token; no redundant note. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

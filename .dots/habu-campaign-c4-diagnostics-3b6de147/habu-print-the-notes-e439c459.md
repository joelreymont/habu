---
title: Print the notes after a record with no place
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T22:31:41.826787+01:00"
closed-at: "2026-11-16T04:07:34.873985+01:00"
---

Problem: src/core/rejection.f END-TEXT left a dropped label (DROP-PART, an AT whose token no source holds) open, so the next note's heading and the record's end were dropped too: the note printed as a bare line and --json-errors wrote invalid JSON (E-TRUST-UNRESOLVED, check.f's E-MISSING-SOURCE and E-UNREADABLE-SOURCE of a named input). Acceptance: such a record prints each note with its heading and closes, in prose and JSON. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

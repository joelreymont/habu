---
title: Reject a named input that is no file as the load does
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:46:57.223396+01:00"
closed-at: "2026-11-16T04:07:34.642937+01:00"
---

Problem: tools/check-core.f CHK-NO-INPUT places its record at a path no source holds, so its label ('not a file'/'not found') drops and a word note appears; a directory gives E-MISSING-SOURCE rc 70 where the load gives E-UNREADABLE-SOURCE 'is a directory' rc 74 (re-review 9b finding 7). Acceptance: check.f prints the load's record and status for a missing file and a directory. Depends: none. Claim: agent=W148 workspace=.jj-ws.noindex/jerry-w148

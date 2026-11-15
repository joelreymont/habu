---
title: Help an unreadable file by its errno
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:46:57.239922+01:00"
closed-at: "2026-11-16T04:07:35.897914+01:00"
---

Problem: src/core/include.f's unreadable-file record helps 'check that the file exists and is readable' for every errno, which fixes neither 'is a directory' nor 'a directory in the path is a file'; tools/source-discovery.f READ-THROW builds its own copy of the record with another help (re-review 9b finding 8). Acceptance: each errno's record helps that case, from one writer. Depends: none. Claim: agent=W149 workspace=.jj-ws.noindex/jerry-w149

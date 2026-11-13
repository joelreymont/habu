---
title: ATOMIC-TARGET overwrites the path RESERVE-SIBLING answered
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:08:45.270913+02:00"
---

Problem: src/lib/fs-mutate.f ATOMIC-TARGET (:502) on a new name copies its basename into ATOMIC-PATH (:511), the buffer whose path RESERVE-SIBLING answered, so a REPLACE-STAGED fill that calls ATOMIC-TARGET renames a garbage path. No current caller hits it. Acceptance: ATOMIC-TARGET leaves a reserved sibling's path intact; a src/lib/fs-mutate-test.f case that calls ATOMIC-TARGET inside a REPLACE-STAGED fill fails before the fix. Depends: none. Claim: unassigned.

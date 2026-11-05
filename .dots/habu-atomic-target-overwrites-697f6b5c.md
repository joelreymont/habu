---
title: ATOMIC-TARGET overwrites the path RESERVE-SIBLING answered
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:08:45.270913+02:00"
---

Problem: src/lib/fs-mutate.f ATOMIC-TARGET (:527) on a new name copies its basename into ATOMIC-PATH, the buffer whose path RESERVE-SIBLING answered, so a REPLACE-STAGED fill that calls ATOMIC-TARGET renames a garbage path (probe /Users/joel/.cache/tmp/rev-errno/probe/probe.f). No current caller hits it. Found by the ERRNO review. Acceptance: ATOMIC-TARGET leaves a reserved sibling's path intact; a fs-mutate-test case that calls ATOMIC-TARGET inside a REPLACE-STAGED fill fails before the fix. Files: src/lib/fs-mutate.f, src/lib/fs-mutate-test.f. Verify: fs-mutate-test on macOS and spark. Depends: none. Ownership: unassigned. Claim: unassigned.

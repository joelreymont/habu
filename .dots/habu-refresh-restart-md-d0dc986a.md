---
title: Refresh RESTART.md for the guard-page engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:41:34.105039+03:00"
---

Problem: RESTART.md is cedar's handoff from the guarded-engine period: it describes workspaces that no longer exist (.jj-ws/cedar-* were forgotten on 2026-09-15), a 95-minute build and a 12.8 MB engine, and the per-transfer guard contract that guard pages replaced. Anyone restarting from it would follow a stale procedure. Acceptance: RESTART.md rewritten for the current line (guard pages, the 6.2 MB engine, build and gate times, the lane and dot state) or deleted in favour of docs/bootstrap.md and LESSONS.md if it duplicates them; the choice recorded in the commit. Files: RESTART.md. Verify: read against the line head. Depends: none. Ownership: hazel line. Claim: unassigned.

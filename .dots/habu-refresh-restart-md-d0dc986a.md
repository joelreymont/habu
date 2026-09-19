---
title: Refresh RESTART.md for the guard-page engine
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T11:41:34.105039+03:00"
---

Problem: RESTART.md is cedar's handoff from the guarded-engine period: it describes workspaces that no longer exist (.jj-ws/cedar-* were forgotten on 2026-09-15), a 95-minute build and a 12.8 MB engine, and the per-transfer guard contract that guard pages replaced. Anyone restarting from it would follow a stale procedure. Acceptance: RESTART.md rewritten for the current line (guard pages, the 6.2 MB engine, build and gate times, the lane and dot state) or deleted in favour of docs/bootstrap.md and LESSONS.md if it duplicates them; the choice recorded in the commit. Files: RESTART.md. Verify: read against the line head. Depends: none. Ownership: hazel line. Claim: unassigned.

Claim: alder, .jj-ws/alder-restart-doc on 1afd910c. The intervening refresh still
hard-coded 6406e0b0, 402 suites, a 5,832,896-byte engine and an obsolete lane
queue. Current evidence is 1afd910c, engine 31be3fb0 (4,456,640 bytes), DD
460/460 in 7m49s; another dated snapshot would immediately go stale again.

Keep RESTART.md as a short navigation index to the live integration bookmark,
dot tracker and subject documents. Remove the duplicated hashes, timings and
lane inventory. docs/roadmap.md's adjacent C1 paragraph now follows those
pointers instead of claiming neither rebuild route works and attributing a
stale qualification statement to RESTART. LESSONS.md is retired; the index
points to bootstrap, gate, debugging and language docs instead. No code change.

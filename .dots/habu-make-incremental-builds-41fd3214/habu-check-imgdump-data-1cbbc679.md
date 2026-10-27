---
title: Check imgdump --data cell offsets
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T05:44:29.557302+02:00"
---

Problem (observed, unverified): tools/imgdump.f --data <off> on an ARM grid image seemed to read one cell high: --data $2d58 printed the text size (TEXT-SIZE-CELL $2D50) and $2d60 printed DPHW (DPHW-CELL $2D58). Acceptance: reproduce on a built image; fix the offset base or show the reading is right. Files: tools/imgdump.f. Verify: dump a known cell of bin/hb. Depends: none. Claim: unassigned.

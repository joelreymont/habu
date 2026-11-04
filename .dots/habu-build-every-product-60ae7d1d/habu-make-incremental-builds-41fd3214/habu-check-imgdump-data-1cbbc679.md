---
title: Check imgdump --data cell offsets
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T05:44:29.557302+02:00"
closed-at: "2026-10-31T10:12:09.146215+02:00"
close-reason: "Fixed: the arm64 DATA stream opens with the DATA extent cell, so --data read one cell early. On a saved app image: $2d50 now prints 6253792 (= otool __text size), $2d58 prints 0 (DPHW zeroed by SND-ZERO-LIVE). Cells past CALLMAP-OFF refuse 'not stored at its offset' (74); SNAP? refuses a short stream; offset checks no longer overflow. Commit 'Read arm64 imgdump --data past the extent cell'. x86-64 not probed; no test runs --data."
---

Problem (observed, unverified): tools/imgdump.f --data <off> on an ARM grid image seemed to read one cell high: --data $2d58 printed the text size (TEXT-SIZE-CELL $2D50) and $2d60 printed DPHW (DPHW-CELL $2D58). Acceptance: reproduce on a built image; fix the offset base or show the reading is right. Files: tools/imgdump.f. Verify: dump a known cell of bin/hb. Depends: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-imgd (brief carl-dots/.out/carl-file/brief-imgd.md).

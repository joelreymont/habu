---
title: snapshot restore copies the maps before validating them
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.237203+02:00"
---

Problem: habu2.f:5864 EM-SNAPSHOT-RESTORE and EMIT-CALLS/EMIT-ADDRS validate image bytes against the maps before rewriting, but EM-SNAPSHOT-COPY-DATA copies [0, datalen) over the live DATA including the maps themselves before they are walked, so a forged image chooses both the map and the bytes it describes; the trailer checks (5910-5913) bound sizes only; codesign covers macOS, src/os/linux/sign.f is a no-op. Not verified by execution. Acceptance: a ruling on the threat model; if in scope, maps read and validated from the image before any copy into DATA, with a forged-image test refused. Files: src/habu/habu2.f, src/habu/snap-lib.f, test/. Verify: the test. Depends: none. Ownership: snapshots. Claim: unassigned.

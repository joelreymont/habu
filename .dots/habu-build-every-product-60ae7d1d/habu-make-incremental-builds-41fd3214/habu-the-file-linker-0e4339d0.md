---
title: The file linker compares the whole data space after every file
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T21:26:33.328138+02:00"
---

Problem (measured 2026-11-10, tools/build-profile.f): src/habu/linker.f COMPARE-RANGE (:1609, run by FIND-STORES :1638 over EACH-RANGE after each file's compile) compares every cell of the data space with its copy to find a store into an earlier file's data, so its work grows with files times data. It is 1461 inclusive samples of 37,215 (3.9%) in a cold Wasm build of Maki's viewer and 14,994 of 156,737 (9.5%) in hb's own build; most of cell-view's self samples (705 of 1975, and 6792 of 11,348) are its calls. REC-CHANGED? (:1519) does the same over every earlier record. Acceptance: the cause of the share is confirmed by a count of cells compared per file, and a cheaper way to find a file's stores into earlier data is designed (it must find every store FIND-STORES finds today); then hb's own build's profile shows the share fallen. Files: src/habu/linker.f. Verify: the profile of hb's own build; warm bytes equal cold. Depends: none. Ownership: none. Claim: unassigned.

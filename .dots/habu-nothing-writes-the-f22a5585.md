---
title: Nothing writes the X64AOT01 table tools/image-size-lib.f reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:51:27.644319+02:00"
---

Problem: with src/habu/aot-x64.f deleted (habu-nothing-loads-src-66ea20d9), tools/image-size-lib.f X64-AOT-LOCATE, X64-AOT-FITS, X64-AOT-U32, X64-AOT-ROWS, the X64-AOT* variables and the X64-AOT branches of X64-CLASSIFY and X64-BUDGET parse a table only that file wrote; src/habu/aot-x64-format.f has no other user; the comments at aot-x64-format.f:1-2 and image-size-lib.f:37-39 describe a stripped x86 image that no longer exists. Found by the review of the deletion. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the reader, its comment sentence, the require at image-size-lib.f:15 and aot-x64-format.f are gone, and a region-less x86 image is refused the way the other malformed x86 images are. Not determined: whether src/habu/kernel-x64.f:5900-5901 ('so a stripped image can retarget the does> clause') still names a live consumer (src/habu/link-x64.f:542 reads the recorded sites). Files: tools/image-size-lib.f, src/habu/aot-x64-format.f. Verify: tools/image-size-lib.f loads and its test passes. Depends: habu-nothing-loads-src-66ea20d9 landed. Ownership: the x86-64 image tools. Claim: unassigned.

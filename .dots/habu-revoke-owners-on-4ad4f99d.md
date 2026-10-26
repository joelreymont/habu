---
title: Revoke owners on an x86-64 negative allot
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T14:44:40.794824+02:00"
---

ARM BALLOT (src/habu/habu1.f) calls the checker owner's GRID-REVOKE callback (NCOMP-DISPATCH:TARGET-DECL-CELL, CHECKER-OWNER-ABI:GRID-REVOKE-OFF) when allot's count is negative, and throws -8574 when the owner has no callback; the x86-64 allot body (src/habu/kernel-x64.f ALLOT-BODY) only moves DP. Found by the allot zeroing lane. Match ARM on x86-64 and test it on an x86-64 host.

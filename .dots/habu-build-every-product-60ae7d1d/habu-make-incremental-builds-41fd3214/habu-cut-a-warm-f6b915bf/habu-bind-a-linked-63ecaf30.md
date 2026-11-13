---
title: "Bind a linked object's arch once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.123757+02:00"
---

Problem: src/habu/linker.f EM-ARCH (:3487) calls NSHADOW:BINDING (src/compiler/native/shadow.f:456) for every call row, and each runs NFETCH-CHECK:TAGS (src/compiler/native/fetch-check.f:125) over every tag row: about 80 ms of a warm engine build (inclusive, so unconfirmed). Acceptance: the arch is found once per object; the time is measured before and after; warm bytes equal cold. Depends: none. Claim: unassigned.

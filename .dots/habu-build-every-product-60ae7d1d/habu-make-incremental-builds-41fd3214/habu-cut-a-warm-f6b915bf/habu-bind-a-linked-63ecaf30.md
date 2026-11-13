---
title: "Bind a linked object's arch once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.123757+02:00"
---

Problem: src/habu/linker.f EM-ARCH (:3487) calls NSHADOW:BINDING (src/compiler/native/shadow.f:456) once per row of CALL-CK, ADDR-CK, SLOT-CK and EMISSION-CK, and each runs NFETCH-CHECK:TAGS (src/compiler/native/fetch-check.f:125) twice (OPEN-CK's typed MATCH on SH-STATE, shadow.f:106, and the typed SH-BIND fetch): 355,357 calls and 91-152 ms per warm engine build. Acceptance: the arch is found once per object; the time is measured before and after; warm bytes equal cold. Depends: none. Claim: unassigned.

---
title: Clear unused newly owned literal storage
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T01:35:56.725249+03:00"
---

Native generation comparison finds an ASLR-dependent cell at window offset 0x195eb8. Translating through the seeded window base locates NSTR:BOOT-ARENA, whose owner has zero rows and zero used bytes. The earlier fixed-header diagnosis omitted that base and was wrong; remove its ineffective capture exclusion. Initialize newly owned literal metadata and arenas, preserving imported borrowed arenas and every live literal. Verify exact native generation convergence and existing literal, snapshot and native-image E2Es, plus independent review. Large differences between the first two checker generations also include handle-number shifts from the changed checker; distinguish that transition from same-host stale bytes.

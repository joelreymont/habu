---
title: Move the remaining checker arenas into the region
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.438521+02:00"
blocks:
  - habu-hold-per-def-8d5c52f1
---

Lane 2b: the rest of DECOUPLED-ARENA-SNAP-RESET (checker.f ~15534-15557: CP, QE*, SCOPE-*, ATOM*, PARAM*, PARGP, PARAM-SCR, VRI-AK, SPA) and WFS (~17761-17768) onto SR-GROW inside spans; delete ARENA-SNAP-BOOT, ARENA-SNAP-BOOT-PTRS, DECOUPLED-ARENA-SNAP-RESET and their *-BOOT buffers. Brief to write after 2a lands. Lane brief .jj-ws.noindex/carl-scr2/.out/brief-2b.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-scr2.

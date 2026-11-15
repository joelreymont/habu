---
title: Delete the surviving native-* engine names
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.556282+02:00"
closed-at: "2026-11-04T11:48:49.753653+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (docs/design.md \"Machinery being replaced\"). native-runtime.f provided rows exist for out-of-date hosts and the reloaded prefix; cross-build and hard cutover remove them."
---

After B1 (habu-build-a-tree-d00185b1: the host reloads the tree's whole prefix), nothing here needs a name; it goes. Hard cutover.

- Joel 2026-10-29: NATIVE-HOST-CELLS, NATIVE-OBS-CELLS and NATIVE-OBS-EXT fold into LAYOUT (src/habu/layout.f) as ordinary DATA cells; the old-host getters native-origin-cell, native-unit-cut-cell, native-pending-opaque? and NATIVE-BODY(-ABI) are deleted wherever a layout cell serves.
- Lead decision, from Joel's "why do we need this": src/habu/native-runtime.f is deleted. The engine's own top file requires its parts (compiler, REPL, debugger) as any program does. The provided rows exist only for out-of-date hosts and lose their reason with B1; the per-target branches move into the src/os files; CAPTURE-PREPARE and SEAL move to the build driver that saves the image; tools/manifest-lint* polices the hand list and goes with it.

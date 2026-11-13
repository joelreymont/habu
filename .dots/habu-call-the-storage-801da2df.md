---
title: Call the storage clear without a defer
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:20:59.753517+01:00"
---

Problem: STORAGE-CLEAR-XT (src/core/layout-buffer.f:199-203) is a defer that src/core/layout-buffer-address.f:4-7 installs once and then undefines; nothing swaps it. It exists only because layout-buffer.f loads in the CHECKER part (src/habu/boot-files.f:78) before src/habu/address-cells.f (:113), whose ADDRESS-CELLS:REMOVE-SPAN LDEFER-RETIRE (layout-buffer.f:264-268) calls through it. A hook or cell stands only for what must be swapped, never for load order (docs/design.md:51). Acceptance: LDEFER-RETIRE clears its old cells' declarations by a plain call; no defer or cell remains for it; DEFER-LAYOUT-BUFFER programs build byte-equal and run as before. Depends: none. Claim: unassigned.

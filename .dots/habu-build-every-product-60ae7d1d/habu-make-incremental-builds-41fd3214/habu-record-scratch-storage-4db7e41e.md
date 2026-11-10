---
title: Record scratch storage as byte ranges
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:03:49.401462+02:00"
blocks:
  - habu-move-every-scratch-4ca80ba0
  - habu-emit-wasm-from-0d5dab47
---

Problem: the engine records scratch as one address-cell row per 8 bytes (src/habu/layout.f:1779-1790, bit 62), so a cell with a pointer kind cannot also be scratch (rc 99, src/habu/habu2.f:330) and every reader walks a row per cell; the file-end reset costs 4% of a cold engine build. Acceptance (design .jj-ws.noindex/jerry-wasm/.out/scratch-declared-design.md, sections 2 to 4, approved by Joel 2026-11-10): one row per SCRATCH-ALLOT call, offset and byte length; whatever lies inside a range is not carried; the object lists ranges and cell rows lose kind 2 (hard cutover); the object writer, the store check, the link, the file-end reset and the program writer read ranges; scratch-cell-mark, XTCELL-SCRATCH-TAG and the kernel bodies are deleted. Waits for capture's last caller (tools/wasm-build.f) to move to the linker, so no per-cell path is kept beside the ranges; open questions the design lists under 'Not determined' are settled when it is briefed. Verify: engine built twice equal, suite, chain. Depends: the conversion dot; habu-emit-wasm-from-0d5dab47. Ownership: unassigned. Claim: unassigned.

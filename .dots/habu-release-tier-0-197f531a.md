---
title: "Release tier 0's control buffers before an image save"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:24:01.841801+02:00"
---

Problem: src/habu/t0-label.f CF-KIND, CF-AT, LV-* and LEAVES (:65-72) and src/habu/t0-wasm.f CF-KIND are DYNAMIC-BUFFERs no word releases, while docs/forth-card.md says of a DYNAMIC-BUFFER 'release before an image save' and both generators register a CAPTURE-PREPARE for their sinks only. Acceptance: first show whether a saved image or a linker-built hb can carry a reserved one (a compile, then the save); if it can, the registered prepare releases them; if no save path remains, the dot closes with that fact. Files: src/habu/t0-label.f, src/habu/t0-wasm.f. Verify: the reproducer. Depends: none. Ownership: unassigned. Claim: unassigned.

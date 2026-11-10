---
title: State in memory.f that a fresh mapping reads 0
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:52:43.577709+02:00"
---

Problem: the JSON:VALUE= review. JSON:VALUE='s walk (src/lib/json/equal.f) and other callers of ALLOC-SPAN rely on a fresh mapping reading 0, and src/lib/memory.f does not state it. Acceptance: memory.f's header or ALLOC-SPAN's comment states the rule and the fact that proves it on each target (anonymous mmap on macOS and Linux, memory.grow on Wasm), after checking each target's implementation; if a target does not zero, that is a defect dot. Files: src/lib/memory.f. Verify: read the three implementations. Depends: none. Ownership: memory.f. Claim: unassigned.

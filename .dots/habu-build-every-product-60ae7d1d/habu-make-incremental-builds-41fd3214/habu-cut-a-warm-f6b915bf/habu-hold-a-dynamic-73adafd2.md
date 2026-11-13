---
title: "Hold a dynamic buffer's base across a loop"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.099734+02:00"
---

Problem: every generated accessor of a DYNAMIC-BUFFER fetches its base on each access (src/core/layout-buffer.f:544) because growth can move the mapping (src/core/dynamic-storage.f:138), and checked code cannot hold the base across calls: 41.7M DYNAMIC-STORAGE:ADDRESS calls per warm engine build, among them hash probes (src/habu/linker.f SLOT-OTHER? :775) and sort compares (src/habu/link-map.f MAP-BEFORE? :141) whose loops never grow the buffer. Acceptance: a design that lets checked code read a buffer it cannot grow without a base fetch per access, with no unchecked hold across a growth. Depends: none. Claim: unassigned.

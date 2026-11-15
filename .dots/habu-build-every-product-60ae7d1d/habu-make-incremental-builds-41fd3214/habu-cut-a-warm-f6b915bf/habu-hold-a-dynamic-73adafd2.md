---
title: "Hold a dynamic buffer's base across a loop"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.099734+02:00"
---

Problem: a DYNAMIC-BUFFER's element child fetches its base on each access (src/core/dynamic-storage.f ELEMENT :199) because growth can move the mapping (GROW :120), and checked code cannot hold the base across calls: 41.7M base fetches per warm engine build (counted as DYNAMIC-STORAGE:ADDRESS calls, before ELEMENT), among them hash probes (src/habu/linker.f SLOT-OTHER? :792) and sort compares (src/habu/link-map.f MAP-BEFORE? :141) whose loops never grow the buffer. Acceptance: a design that lets checked code read a buffer it cannot grow without a base fetch per access, with no unchecked hold across a growth. Depends: none. Claim: unassigned.

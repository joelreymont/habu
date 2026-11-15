---
title: "A cross build reads the target's code with arm64's address decoder"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T21:01:24.063444+02:00"
---

Problem: src/habu/linker.f EXTENTS! (:2070) -> EM-DATA (:2002) reads each DNAME-ALLOC record's shadow emission through ADDRESS-CARRIER:CHAIN-SIZE, arm64's movz/movk grammar, while a cross build's shadow holds the target's code. On another machine's code it answers 0, the record gets no body anchor and its data falls into the previous extent or the head (found by reading). Harmless while a cross build writes and links no object. Acceptance: an acceptance case of the step that makes a cross build write objects: extents come from the target's own encoding or from the record, not the host's decoder. Depends: cross builds writing objects. Claim: unassigned.

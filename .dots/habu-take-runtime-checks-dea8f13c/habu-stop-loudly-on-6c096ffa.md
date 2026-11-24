---
title: "Stop loudly on a damaged object's structure"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T16:19:51.829132+01:00"
blocks:
  - habu-treat-a-malformed-ed4eb79b
---

Problem: src/habu/linker.f PIECE-CK and the -CK words it calls (HEADER-CK, ENTRIES-CK, CODE-CK, DATA-CK, FACTS-CK, ACTS-CK, ROWS-CK) set BAD for a malformed object, and PART-FRESH? then compiles the part, so a damaged object is compiled around in silence; docs/architecture.md "Builds": a damaged object stops the build. One BAD cause is no damage: HEADER-CK's key, where a.f.2 or a.f+1 meets another load's object. Acceptance: each BAD cause only damage can produce ends the build in the E-DAMAGED-OBJECT record naming the object file; the key mismatch still compiles. Depends: habu-treat-a-malformed-ed4eb79b. Claim: unassigned.

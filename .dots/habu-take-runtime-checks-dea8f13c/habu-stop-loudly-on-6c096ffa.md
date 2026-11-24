---
title: "Stop loudly on a damaged object's structure"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T16:19:51.829132+01:00"
---

Problem: a build cannot tell an object hb wrote from one changed outside hb. src/habu/linker.f PIECE-CK and the -CK words it calls (HEADER-CK, ENTRIES-CK, CODE-CK, DATA-CK, FACTS-CK, ACTS-CK, ROWS-CK) set BAD for a malformed object, and PART-FRESH? then compiles the part, so damage is compiled around in silence; some damage ends in a silent compile without BAD, a throw, or a read past a buffer (SLOT-CK). A type entry's text (src/core/type-family.f ER-READ and the readers it calls) is checked number by number, and damage that stays in range passes: a swapped field slot builds and runs wrong, a changed variant name or tag blames the user's source. docs/architecture.md "Builds": a damaged object stops the build. Acceptance: damage of any kind to an object or to the facts hb's image carries ends the build in one E-DAMAGED-OBJECT record naming the object and the recovery; a correctly written object that no longer fits the build (an edited source, a removed word, another load's key) still compiles; each per-value check that remains states why; a test corrupts a saved object on disk and sees the record. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-sum (the checksum and the stop; the deletions follow on it).

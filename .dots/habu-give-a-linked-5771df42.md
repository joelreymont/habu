---
title: "Give a linked clause's duplicate the cold build's line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:30:11.369599+02:00"
---

Problem: a warm build that links h.f, whose does> definer creates FILE-OBJECTS-CL;does, refuses a later file's FILE-OBJECTS-CL;does with 'h.f: duplicate definition' and no line, where the cold build says 'h.f:1:' (test/file-objects.f CLAUSE-CASE CL3). An object records only names its source spells (src/habu/linker.f SPELL-TEXT, names end at whitespace, colon or quote), so h.f's object never records the clause name, stays valid, and MAKE-DEF's link-time check refuses without a source line. Acceptance: a does> definer's object records its clause name, the warm build recompiles h.f and refuses as cold does, and a test still reaches MAKE-DEF's DEF-DOES-NAME check or that check is deleted as unreachable. Depends: none. Claim: unassigned.

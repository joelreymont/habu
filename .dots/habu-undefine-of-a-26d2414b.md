---
title: "undefine of a global inside a package tombstones P:X"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T11:58:19.992227+02:00"
---

Problem: 'undefine X' inside package P, where X is a global, tombstones P:X by spelling in the checker while the engine retires the global X (src/core/checker.f CHECKER-UNDEFINE / UNDEFINE-SYM). A link replays the same identity, so cold and warm agree, but the checker keeps X's facts live and refuses nothing for a P:X that never existed. Acceptance: the checker undefines the symbol the engine retired; a program that undefines a global inside a package and then calls X is refused as the engine refuses it. Files: src/core/checker.f, src/habu/interpret.f (undefine route). Verify: that program through hb --load, rc and message. Depends: none. Ownership: src/core/checker.f. Claim: unassigned.

---
title: "Treat a malformed object's facts as stale"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T18:45:09.566911+01:00"
---

Problem: a type entry's or undefine's text crosses a piece or image file (src/habu/linker.f:5417), but its reader (type-family.f ER-HEAD/ER-FIELD about :3600-3660; checker.f READ-UNDEFINE :17950-17954) passes raw numbers on and dies in TFAM-DECL, TFAM-PK!, TFAM-LAYOUT!, LAY-ADD, PF-ADD; linker.f FACTS-CK (:3798) checks only the row kind. A corrupt object on disk reaches 27 internal dies (census .out/census/dies-b.md class REJECT, a.md class OTHER). Acceptance: the reader checks each field once and FACTS-CK marks such an object BAD, so a warm build recompiles it as a cold build would; the dies in the declaration words go. Depends: none. Claim: unassigned.

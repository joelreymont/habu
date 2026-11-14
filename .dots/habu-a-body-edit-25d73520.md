---
title: A body edit rewrites every object whose walk reaches the word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:52:55.069874+02:00"
---

Problem: src/habu/linker.f lays a WORDS row for every word a part's reach walk reaches (SEE :1289) and WORDS-CK (:3189) refuses the object when that word's time changed. A body edit of CORE-FOLD-C (src/core/util.f) rewrites 127 objects besides util.f's two. Four spell it. 29 reach it by word edges through seeds that run the code just compiled from the source being built, so they are true dependencies. 94 reach it only through a WALK-STORERS (:1307) whole-piece span, such as the storer span of prefix-boundary.f (its `undefine FIRST-RECORD-XT`, :28). The 94 need a store followed only into a reached extent, changer records included; that rule is sound only once the language bounds a read to its extent (habu-refuse-an-unbounded-05e30f47, as habu-a-store-brings-1ba604bf records). Not known: whether the 94 ever execute a word on the far side of a storer span. Acceptance: a design, then a warm build after the edit rewrites none of the 94, and warm = cold. Depends: habu-refuse-an-unbounded-05e30f47. Claim: unassigned.

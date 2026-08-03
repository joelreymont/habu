---
title: Make clobber tests pressure-sensitive
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T21:14:07.558308+02:00"
---

Destruction review of NCLOB, medium. test/compiler/native-clobber.f is scheduled and drives real migrations, but: WIDEN-CASES (:117-126) calls NCLOB:RECORD directly on synthetic addresses $10000.. so the widen rule is never exercised through the real publication seam (where per the critical findings it is wrongly ordered); NARROW-CASES (:227-244) measures data-stack traffic with the recorded set 2 bits against an 8-register pool — saturated, so GPR-ROOM 6 vs 7 moves no count and deleting a register from NCLOB:RECORD passes every scheduled gate. Add: (a) a widen case through the real seam — migrate, reclaim slot, republish wider onto it, assert refusal happens BEFORE the dictionary record moves; (b) a narrowing case with live values exceeding GPR-ROOM so a dropped register moves the counts; (c) a default case — a caller whose callee's slot was reclaimed must save the full set. Depends on the fixes in habu-tie-row-lifetime and habu-refuse-the-clobber dots; write these tests with those fixes.

Claim: agent=row-lifetime workspace=.jj-ws/habu-tie-native-rows-2103f90f

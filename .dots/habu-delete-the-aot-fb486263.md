---
title: Delete the AOT capture chain no program reaches
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T16:01:09.009088+01:00"
---

Problem: since tier 1's compiler went (c7dec3dd deleted tools/native-emit-x64.f), src/habu/aot-capture.f, aot-arm.f, aot-file.f, aot-shadow.f, aot-owned.f and link-x64.f are loaded only by test/aot-native-does.f (in no suite) and test/x86-64-link-records.f (gate-x64-cases.f only, and it requires the deleted native-emit-x64.f); aot-owned.f and link-x64.f by nothing else. The checker and type-family words only they reach stay: CHECKER-PAYLOAD-*, ASIG-GRAPH-CHECK and CK-GRAPH-CHECK (whose CK-GRAPH-CON? 'con 0=' branch is unreachable, since ASIG-GRAPH-NODE copies constructors from the live table), CHECKER-ASIG-KNOWN?, -MISSING?, -ROW-FOR and -ARM/-DISARM/-ARMED?, and type-family.f's REG-AOT-* walker, whose REG-AOT-NODE-W, -FIELDS-W, -FAM-WALK and -FAM-W-IMPL repeat TFW-FAM over the same live rows; AOT-FILE:MERGE, MERGE-REG and TFAM:REG-AOT-MERGE-INCOMING? have no caller at all. Acceptance: the files, the words only they reach and the tests that only probe them are deleted, or a kept word names its caller from a program entry; hb builds both generations and the suite passes. Depends: none. Claim: unassigned.

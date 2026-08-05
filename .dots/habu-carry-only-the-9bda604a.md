---
title: Carry only the live values across an edge
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T14:37:01.281736+02:00"
---

src/compiler/native/elaborate.f decides what crosses an edge from what is MENTIONED where a call can reach it (CROSS-SCAN, CROSS-N, CROSS-L), not from what is LIVE there. Two places pay for the difference. A local mentioned in one arm of an 'if' whose other arm calls is marked as crossing although the call cannot reach that read, and every counted loop's counters cross every edge of a calling body although only the loops that contain a call can have theirs renamed. Each extra carried value is a class holding a register from the branch to the block, and a class holding a block argument cannot be spilled (dot habu-spill-a-class-f712088d), so the cost lands as register pressure and can turn into E-A64RA-SPILL on a tight budget. The fix is a real liveness pass over the elaborated blocks - or the same answer computed while the skeleton walks - so an edge carries exactly the values read on some path out of it. Owner: NELAB.

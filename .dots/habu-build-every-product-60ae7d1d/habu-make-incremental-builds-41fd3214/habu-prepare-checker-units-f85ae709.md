---
title: Prepare checker units before a hit installs
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T19:49:30.207817+03:00"
closed-at: "2026-10-28T17:13:33.212888+02:00"
close-reason: landed 413581b2; CHECKER-PREPARED? in OBJECT-HIT?
---

A cache hit never throws. Today checker unit prepare (UP-READ-BLOB -> UNIT-TFAM-PREPARE, UP-CTOR-ORD) runs inside INSTALL-OBJECT after OBJECT-HIT? accepted (tools/native-definition-cache.f FILE-UNIT-WITH), so a malformed or unsupported checker slice throws mid-install. Acceptance: every checker slice is prepared before any install side effect; each refusal is a named OBJECT-MISS; a corrupted cached checker blob rebuilds from source. Base: the linearized carl stack tip. Workspace: carl-prep (not yet created).

Census (~/.cache/tmp/carl-prep/census.md): A 46, B 27, C 5 (none reachable by a well-formed object), mixed 7. Lead design: prepare the checker slice inside OBJECT-HIT? as its last check, resolving own-record refs by ordinal (INS-FIRST + item) so prepare needs no installed record; a prepare refusal is a named miss; INSTALL-OBJECT installs the prepared plan after INSTALL-SEGMENT; a miss releases the plan leaving the cold state. Step 1: fail-first fixture (carl-prep, worker running, brief brief-prep-fixture.md). Step 2: the fix, then Astra major review (failure recovery).

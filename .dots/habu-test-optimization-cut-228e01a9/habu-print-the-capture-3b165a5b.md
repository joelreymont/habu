---
title: Print the capture when a subject run times out
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T18:00:48.161771+02:00"
---

Problem: test/compiler/native-div-refusal.f:77 STORE! does `timeout OF E-PROC-TIMEOUT throw ENDOF` on a timed-out SUBJECT:RUN (lib/test/subject.f:90-98). SUBJECT:RUN drains into the row's own OUT/ERR and PROC-CAPTURE-FINISH-OUTCOME returns the drained lengths even on a timeout, so STORE! throws with the partial capture in hand and never stores or prints it: the row dies with -2502 and no label, program or output. GE-RC@/GE-FAIL print GT-OUT$/GT-ERR$, which this path never fills, so the GE-RC@ fix (dot 8beb9952) cannot apply here. Found by review 205 of the gerc lane. Acceptance: on a timeout outcome STORE! stores the lengths and prints T-LABEL$, the program and OUT$/ERR$ before throwing E-PROC-TIMEOUT; a copy of the row with 0 at RUN's TIMEOUT-MS shows that report then -2502 (seen failing first); the row alone still exits 0. Search lib/test/subject.f's other callers for the same timeout arm and fix each the same way, listed in the commit. Files: test/compiler/native-div-refusal.f and any caller the search finds. Not baked.

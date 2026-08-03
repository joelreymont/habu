---
title: "Take the replacement log's capacity refusal before the write"
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T15:05:12.558991+02:00"
---

src/compiler/native/publish.f says at its head that everything which can refuse refuses before the first byte is written, so a refusal leaves the dictionary record exactly as it found it. LOG+ breaks that: it throws E-NPUB-CAP when the replacement log is full, and REPUBLISH calls it AFTER WRITE and RETARGET have already moved the code pointer and pointed the record at the new routine. The 129th republication in a process therefore leaves a word running the chain's code with no log row - which is the only surviving record of what the old emitter produced for that name - and a throw for the caller to interpret. The fix is the one NCLOB:ROOM-CK uses for the same problem in the clobber record: ask whether there is room, and whether the name fits, in the block of checks in front of WRITE, and leave LOG+ as the writer. Both of its clauses are pure functions of the log's own state and the name's length, so both can be asked early. Owners: NPUB.

---
title: Test that an unreadable or cut object compiles its file
status: open
priority: 3
issue-type: task
created-at: "2026-11-07T03:06:15.678041+02:00"
---

Problem (L4 Fable review F5, 2026-11-07): no test exercises READ-OBJECT's stale answer (src/habu/linker.f READ-OBJECT: E-FS-OPEN, E-FS-STAT, E-FS-IO -> the file compiles and its write replaces the object), nor FRESH?'s refusal of a truncated object. Lead's manual probe (2026-11-07, /tmp/carl-probe-b/): a chmod-000 object -> compiled 1, rc 0; ATOMIC-WRITE-FILE keeps the replaced file's mode, so the new object is mode 000 again and the file recompiles on every build until the mode is fixed. Review sweep: 68 truncations of count.f's object -> compiled 1, program identical. Acceptance: test/file-objects.f, through build.f's stop mode (one more LINKER:WITH on an existing out root), cuts one object short and checks compiled 1 and a program equal to the cold build's. Files: test/fixtures/file-objects/build.f, test/file-objects.f. Depends: L4 landed. Ownership: carl (lead), worker lane. Claim: unassigned.

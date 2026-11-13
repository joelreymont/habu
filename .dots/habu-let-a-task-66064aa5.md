---
title: "Let a TASK:+USER row keep its roles"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T14:18:09.011298+03:00"
---

Problem: src/lib/process.f:101 `STORAGE-BYTES TASK:+USERS STORAGE u8` and src/lib/net/tcp4.f:183 `$20 TASK:+USERS IO-STORAGE u8` lay a per-task row as bytes and carve it by hand offsets (process.f's *-OFF constants; tcp4.f's $10, $14, $18), so process.f's PID, RC and the other cells answer `ptr n` and each fd, pid, rc, len and off role is cast where it is used: about twenty cast sites in test/gate-common-lib.f, src/lib/test/subject.f and the three spawn layers. TASK:+USER takes a type, record types included, so a row can be a declared STRUCTURE whose fields carry their roles; public effects are meant to keep their roles.
Acceptance: each row is a TASK:+USER slot of a declared structure with no hand offset; its fields answer their roles and the call-site casts go; the process, tasking and tcp4 suites pass.
Depends: none. Claim: unassigned.

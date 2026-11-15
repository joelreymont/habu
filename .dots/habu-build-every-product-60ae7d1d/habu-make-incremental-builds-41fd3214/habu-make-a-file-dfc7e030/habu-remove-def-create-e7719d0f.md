---
title: Remove def-create, which no definer calls
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.770860+02:00"
closed-at: "2026-11-13T23:19:31.712422+01:00"
---

Problem: the def-create engine primitive (it ends a pending `create`-kind definition) has no caller: src/habu/interpreter.f DEF-CREATE-ROW (:2156-2158) publishes through DEF-ADDR-ROW instead, and no source names `def-create` outside its own rows. Its rows remain: src/habu/prims.f:772-781 (the comment and the EPRIM and EPPRIM rows), src/habu/habu2.f:1603-1605 (DEFWRITE:DEF-CREATE) and its FPRIM-WID row at :2978, and src/habu/kernel-x64.f:5244, the x86-64 refuse row. Comments that name it: prims.f:659, :661 and :711, habu2.f:1551, :1581, :1601 and :1630, kernel-x64.f:5228. TAIL-CLOSE (habu2.f:1585) and ADDR-TAIL, (:1552) stay: the def-cast row and `create` use them.
Acceptance: no source names `def-create`; the comments that named it describe what remains; hb and the x86-64 kernel build and pass as before.
Depends: none.
Claim: unassigned.

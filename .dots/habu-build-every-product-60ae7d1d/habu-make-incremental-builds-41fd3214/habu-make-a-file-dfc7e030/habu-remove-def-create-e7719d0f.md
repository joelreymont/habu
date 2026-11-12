---
title: Remove def-create, which no definer calls
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.770860+02:00"
---

Problem: the def-create engine primitive (it ends a pending `create`-kind definition) has no caller: src/habu/interpreter.f DEF-CREATE-ROW (:2016-2018) publishes through DEF-ADDR-ROW instead, and no source names `def-create` outside its own rows. Its rows remain: src/habu/prims.f:863-872 (the comment and the EPRIM and EPPRIM rows), src/habu/habu2.f:1646-1648 (DEFWRITE:DEF-CREATE) and its FPRIM-WID row at :3365, and src/habu/kernel-x64.f:5890, the x86-64 refuse row. Comments that name it: prims.f:758, :760 and :816, habu2.f:1595, :1626 and :1673, kernel-x64.f:5874. TAIL-CLOSE (habu2.f:1630) and ADDR-TAIL, (:1596) stay: the def-cast row and `create` use them.
Acceptance: no source names `def-create`; the comments that named it describe what remains; hb and the x86-64 kernel build and pass as before.
Depends: none.
Claim: unassigned.

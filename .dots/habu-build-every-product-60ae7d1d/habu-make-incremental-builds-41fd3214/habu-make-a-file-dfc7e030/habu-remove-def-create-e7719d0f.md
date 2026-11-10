---
title: Remove def-create, which no definer calls
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.770860+02:00"
---

Problem: the def-create engine primitive (it ends a pending `create`-kind definition) has no caller: src/habu/definers.f DEF-CREATE-ROW (:783) publishes through DEF-ADDR-ROW instead, and no source names `def-create` outside its own rows. Its rows remain: src/habu/prims.f:921-931 (the comment and the EPRIM and EPPRIM rows), src/habu/habu2.f:4660-4662 (DEFWRITE:DEF-CREATE) and its FPRIM-WID row at :14145, and src/habu/kernel-x64.f:5948, the x86-64 refuse row. Comments that name it: prims.f:813-818, habu2.f:4605, :4640 and :5148, kernel-x64.f:5932. TAIL-CLOSE (habu2.f:4644) and ADDR-TAIL, stay: the def-cast row and `create` use them.
Acceptance: no source names `def-create`; the comments that named it describe what remains; hb and the x86-64 kernel build and pass as before.
Files: src/habu/prims.f, src/habu/habu2.f, src/habu/kernel-x64.f.
Verify: `rg -w def-create src` prints nothing; hb rebuilds on arm64 and passes the suite; the x86-64 cross-build and its suite pass on pulsar.
Depends: none.
Ownership: unassigned. Claim: unassigned.

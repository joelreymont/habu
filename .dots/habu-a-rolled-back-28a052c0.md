---
title: A rolled-back claim leaves a chain head on reclaimed storage
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:24:12.603732+02:00"
---

Problem: src/lib/task.f SLOT-LAST (:1590, the newest slot body, read by HIS :1670 through MAIN-SLOT-OFF :1654) is a PERSISTED-PTR-VARIABLE that SLOT-LAY (:1598) stores at a claim. A claim inside an evaluate that later throws is rolled back: the dictionary gives the body's storage back, the head still names it, and the next definition lays other data there. In the Mac's ~/.cache/tmp/carl-band/rv/p/rollback.f, after a rolled-back PB, JUNK sits at PB's body, PC's link is JUNK's address, and TASK:HIS on PA answers E-TASK-USER (-3605); TASK-USER-NEXT also stays advanced. Inferred, not run: TASK-CHAIN (:1008, laid by TASK :1557, walked by the x86-64 crash handler) is stored the same way and has the same exposure for a TASK defined in a rolled-back evaluate. Cause: a frame rollback returns dictionary space and does not undo a store the rolled-back code made into a cell below the cut. TASK:HIS stays public (Joel: 'keep it public'), and so does TASK-CHAIN. Acceptance: reproduce the TASK-CHAIN case first; after any rolled-back TASK or +USER definition each chain head names a live body, fixed in the rollback layer or in how a definer records its chain, not by a check in each walker; a test in src/lib/task-test.f holds each reproducer. Consumers: none meets it. Depends: none. Claim: unassigned, owner carl.

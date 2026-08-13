---
title: Guard the back-half migration transaction with a test
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T16:46:06.585193+02:00"
---

The exceptions design probe (6ceb7667) measured that a refusal DEEP in the chain - E-A64RA-PRESSURE (-8508), after elaboration succeeded - leaves exactly the marks a success leaves (ndict/here/cp identical, no stale elaborator record). The front half is guarded (test/compiler/native-migrate.f:1765 HELD-REFUSAL-CASE covers pre-elaboration refusals) but NO standing test covers a back-half refusal; the probe's -8508 row is unguarded. Add a back-half refusal case to the same suite: a subject that passes elaboration and refuses in regalloc, asserting no publication, no record, name free, marks clean. Probe fixture shape at /private/tmp/hb-exc/fx/txprobe*.f. Files: test/compiler/native-migrate.f. Depends: none.

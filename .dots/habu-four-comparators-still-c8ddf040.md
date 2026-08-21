---
title: Four comparators still discard their evidence
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T11:38:43.337849+02:00"
---

The residue from 395eb72a's landing (master 4d024e02, recorded in the fix commit's message): GT-TIMEOUT, GT-RC-NONZERO, GT-STDOUT-HAS and GT-STDERR-HAS still reduce their observed value before reporting - the same defect class GT-RC=/GT-STDOUT=/GT-STDERR= just closed, outside that dot's named scope. Fix per the landed shape: comparison inside the reporting word so operands survive to print, golden-output regression per the GTT-TEST-CHECK-DETAIL precedent. Small, mechanical against a worked example; the runner's reporting surface is inconsistent until done.

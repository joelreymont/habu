---
title: Snapshot payload pays the dict band growth
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T04:15:20.721543+02:00"
---

Residual from the DICT-CAP 65536 lift (bake-chain-21, 2026-08-17): snap-lib.f:88 writes [SDB, CP), so the +786KB dictionary band rides in EVERY snapshot payload. Noted as real-not-deciding at the ruling; becomes actionable if snapshot size ever gates. Fix shapes to evaluate then: write the dict band sparsely (records end at ndict, the band past it is zeros), or bound the written span by the live watermark rather than DICT-SIZE. Measure first: actual payload delta on a real snapshot before and after the lift.

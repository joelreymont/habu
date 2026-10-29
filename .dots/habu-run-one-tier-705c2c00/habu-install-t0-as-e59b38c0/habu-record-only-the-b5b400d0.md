---
title: Record only the checker facts T0 reads
status: open
priority: 1
issue-type: task
created-at: "2026-10-29T05:20:01.973779+02:00"
---

From the load attribution (jerry-t0ld/.out, zxqqtqpx): tender T0 load 1.238x JIT (needs -0.94G for 1.2x), zz 1.200x. T0:SCAN arms the tape (REC-ON, checker.f:23642); recording words (CALL-FREEZE 14506, RECORDED-STEP 4403, SCAN-REPORT 24971, CWIN-*, EV-*) cost ~1.3G direct on tender plus up to 2.6G inflation in shared checker words. T0's observer drops token events (t0.f:1514). Step 1: census T0's readers of recorded facts and measure a scratch engine recording only those. Step 2: the edit. Other rows: bulk buffer appends ~1.0G tender (byte-buffer.f:204 APPEND-BYTE-RAW), publication 0.92G, T0 tokenizer 0.68G.

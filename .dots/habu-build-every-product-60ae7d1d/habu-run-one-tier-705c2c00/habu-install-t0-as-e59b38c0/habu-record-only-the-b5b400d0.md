---
title: Record only the checker facts T0 reads
status: closed
priority: 1
issue-type: task
created-at: "2026-10-29T05:20:01.973779+02:00"
closed-at: "2026-10-29T12:42:28.074570+02:00"
close-reason: "Superseded by habu-lower-t0-from-b1a1e01e: T0 lowers from the checker's events, so no narrowed fact recording is needed."
---

From the load attribution (jerry-t0ld/.out, zxqqtqpx): tender T0 load 1.238x JIT (needs -0.94G for 1.2x), zz 1.200x. T0:SCAN arms the tape (REC-ON, checker.f:23642); recording words (CALL-FREEZE 14506, RECORDED-STEP 4403, SCAN-REPORT 24971, CWIN-*, EV-*) cost ~1.3G direct on tender plus up to 2.6G inflation in shared checker words. T0's observer drops token events (t0.f:1514). Step 1: census T0's readers of recorded facts and measure a scratch engine recording only those. Step 2: the edit. Other rows: bulk buffer appends ~1.0G tender (byte-buffer.f:204 APPEND-BYTE-RAW), publication 0.92G, T0 tokenizer 0.68G.

Census (jerry-t0rec, prototype commit zyupnroy "Prototype: record only the layout facts T0 reads (unwired)"): T0 reads only the construct call row (CWIN-CELLS via NDICT:CALL-CELLS), MWIN match cells and con pads, CWIN match payload, token ordinals (REC-IX) and the tape's DONE; widths come from the lowering certificate, which recording does not gate. Unread: call freeze for non-construct calls, CW-GLUE, quotation call windows, BWIN, C2-STOW-BORROW, CW-INIT-*, CW-FIELD-SPAN, INTRINSIC-CREATE rows, XPORT-CALL, EVENT!, the token feed. Mechanism: REC-CALLS beside REC-ON, CHECKER-TAPE:ARM-LAYOUT, CON-RECORDED-STEP. Source-loaded T0 cannot reach ARM-LAYOUT without a new owner-ABI slot. Decision (lead): wire it in A6, where T0 is built into the engine and calls the checker directly; no throwaway owner slot. Not yet measured.

---
title: Separate AOT records from owner freeze rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T12:47:59.547273+02:00"
blocks:
  - habu-owner-seal-persist-1f23e205
---

Full context: src/habu/aot-capture.f captures 48-byte records in AOT-REC-BUF, proves them, then OWNER-WID-CAPTURE:FREEZE writes up to 256 eight-byte owner rows into that same buffer before ACAP-BOOTRUN-NAME+ performs its final record-name scan. With 256 rows the first 2048 bytes overwrite records 0 through 41 and part of record 42; current boot targets happen to be later, so the gate passes by ordering accident. Cause: two independently live artifacts share one untyped region and the pipeline has no last-use or phase proof. Fix: give OWNER-WID-EMIT a dedicated 2048-byte typed freeze buffer and use it for freeze, revalidation, and emission; keep AOT-REC-BUF exclusively for records; run ACAP-PROVE-RECS after owner freeze and add a regression whose boot target lies in the overwritten prefix. Acceptance: prefix-target capture, boot-manifest lookup, owner revalidation/emission, AOT closure, snapshot, forced fixpoint, and native gates pass; an intentional shared-buffer mutation fails the regression. Long-term prevention remains in habu-add-bounded-host-b40b048f and habu-add-linear-capture-172b29da.

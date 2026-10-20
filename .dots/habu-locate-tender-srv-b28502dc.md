---
title: Locate Tender server AOT refusal
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T19:39:29.631400+03:00"
---

Kevin/Stuart reduced the public stripped-server refusal to raw comma cells: `/Users/joel/.cache/tmp/stuart-habu-unaligned-aot/raw-two-cell.f` declares `create STUART-RAW 1 , 2 ,` then empty MAIN; public e620 hb-build exits 70 at an offset 4 modulo8 with value 2 shifted left32. Tender's retained `/Users/joel/.cache/tmp/tender-downloads-QVbbaU/aot-probe/probe.f` requires server/scrape/slot.f and declares 123/456/789 RUN-MAPPED buffers: one/two pass, three refuse with the same shifted value. RUN-MAPPED at server/scrape/slot.f:167 owns the raw byte-count/index pair. Implement and qualify explicit CELL-BODY metadata at Habu's storage boundary, preserving raw-byte scanning and actual-pointer refusals; Tender owns adoption after the public API/revision. No generic comma tracking, value whitelist or application workaround. Existing cell-body image E2E is the bounded regression; obtain independent Astra review and required native/convergence gates before release. Full server qualification follows caller adoption. Public 600eb464-4849828a contains no CELL-BODY; Kevin's later unchanged 15a6fd6d server attempt stayed CPU-active until its 600000ms builder timeout, exit 67, producing no binary. This does not identify a new owner or establish that the pending fix is public. Keep full retries and declaration edits held; no further reducer is needed.

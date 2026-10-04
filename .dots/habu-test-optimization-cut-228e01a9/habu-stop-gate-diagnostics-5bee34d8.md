---
title: Stop gate-diagnostics overflowing under load
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T18:18:55.372348+03:00"
---

Batch 4e full gate (jerry-batch4e top aeee6231, load 25-38): native-gate-diagnostics (test/gate-diagnostics.f) ended 'hb: uncaught throw code -2201' (E-STR-CAPACITY), rc 67, right after 'PASS: malformed record contract' (log ~/.cache/tmp/kestrel-r4/b4e-gate/gate.log:1020-1027). Alone it passes on the same engine and on the 4c engine (rc 0, 7 s; ~/.cache/tmp/kestrel-r4/b4e-gate/solo/). dave's round-4b pool run hit the same -2201 in this row (~/.cache/tmp/dave-round4b-release/native-gate.log). Suspect: test/gate-diagnostics-lib.f:516-519 stores the child's whole stderr (GT-ERR$ REC!) and REC-SWAPs a longer repair class into a fixed record buffer; extra child stderr under load (deadline/slow notes) pushes it past capacity. Acceptance: root cause shown by a reproduction that forces the load condition (e.g. the extra stderr) and fails with -2201 before the fix; the record step reads only the record line (or sizes to what it reads) so the case is load-independent; no uncaught throw may end a gate row — a capacity refusal is a named test failure.

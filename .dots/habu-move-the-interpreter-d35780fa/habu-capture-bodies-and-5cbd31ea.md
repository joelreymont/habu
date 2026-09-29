---
title: Capture bodies and string keywords in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.913784+03:00"
blocks:
  - habu-move-tier-1-aacb6029
---

Problem: body capture into `BODYBUF` and the string keywords are assembly (`LBCAP`/`LBCS`, `habu2.f:7509-7527`).
Acceptance: `LBCAP`/`LBCS` semantics into `BODYBUF` (`CAPTURE-PLAIN-STRING`/`CAPTURE-ESCAPED-STRING`/ `CAPTURE-STRING`, `habu2.f:7509-7527`), the escaped forms, the capacity refusal.
Files: `src/habu/definers.f`, `src/habu/outer.f` (compile-mode dispatch), cases beside `test/outer-interpret.f`.
Verify: spark: body-capture and string cases through the Habu loop under the feature cell, including capacity refusal; gate.
Depends: habu-move-tier-1-aacb6029 (I5a).
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
- From I4 design: this leaf also owns a top-level `[:`, which captures a quotation body from interpret mode (dispatched before find, `habu2.f:8339-8362`).

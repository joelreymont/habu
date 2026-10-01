---
title: Capture bodies and string keywords in Habu
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.913784+03:00"
closed-at: "2026-10-01T09:53:35.576145+03:00"
close-reason: "done: src/habu/definers.f captures the six string keywords' text in a body as the engine's CAPTURE-STRING; bin/hb --load test/outer-interpret.f agrees with the engine on 119 cases (9 new: each keyword plain and escaped, a quotation body, refusals, capacity 8001/8002), ThinkPad and spark; top-level `[:` is E-UNDEFINED on both loops."
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

Lead note (2026-09-30, from the I8/I5a design): keeps CAPTURE-STRING (the six keywords captured as one span through `body-append`, plus the escaped forms) and top-level `[:`; Files: `definers.f`. LBCAP/LBCS and the rc-71 refusal move to I5a.

---
title: "Record a code span's length in one word"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T16:44:57.270728+02:00"
---

Problem: the rule for a record's length cell (the emission's size less its trailing return, CODE-SPAN:EXACT of the size when it has none) is written three times: src/compiler/native/publish.f RECORDED-LEN, src/habu/linker.f where a file object's record is replayed, and src/habu/link-data.f TARGET-LEN. Acceptance: one word in src/habu/code-span.f holds the rule and its comment, and the three sites call it; the engine builds twice byte-identical to before. Depends: none. Claim: unassigned.

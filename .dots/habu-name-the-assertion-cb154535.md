---
title: Name the assertion, not its number
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T19:44:58.691831+02:00"
---

Problem: test/compiler/native-inline.f:753 cites 'tools/codegen-compare-test.f assertion 238' as the historical witness of the qualified-spelling resolver bug (dot habu-resolve-qualified-spellings-ec037942). An assertion number shifts every time a case is added to that file - dot habu-adjudicate-bytes-and-df0f14d8 added six byte-column cases before it and moved every later number - so the citation now points at some other assertion. Acceptance: cite the assertion by its T-LABEL text (or by the case word that holds it) instead of by index, and check the tree for other assertion-number citations. Files: test/compiler/native-inline.f, any other file citing an assertion index. Verify: bin/hb --load test/compiler/native-inline.f; bin/hb --load tools/codegen-compare-test.f. Depends: none. Ownership: test/compiler/native-inline.f prose only. Claim: unassigned.

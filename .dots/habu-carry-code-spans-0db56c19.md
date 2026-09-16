---
title: Carry code spans in the payload so stripped rows can go
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T01:17:31.895030+03:00"
---

Problem: the name strip (e74437cf, dot habu-ship-no-dictionary-2fee2dea) leaves a 20-byte record (48 in the booted dictionary) for every private word nothing can ask for, because src/habu/aot-lib.f:344 walks the shipped records to retarget every PC-relative branch when hb-build shakes an application out of the image, and a displacement landing in a span no record covers refuses ('aot: PC-relative target removed or outside closure', exit 74; test/gate-aot-positive.f reds): dropping the rows measured 196,608 bytes (three pages) against the strip's 65,536. Acceptance: the payload carries a span table (start, len per stripped word, 8 bytes each) that aot-lib.f's closure walk reads instead of a record, stripped rows are not shipped, hb-build and test/gate-aot-positive.f pass, the booted dictionary holds no record for a stripped word, engine size measured on a native-runtime engine before and after with the same host class stated; byte fixpoint two generations; test/run.f. Files: src/habu/aot-capture.f, aot-lib.f, aot-decl.f, habu2.f (EM-AOT-REGISTER-RECS), tools/native-build-core.f, test/gate-aot-positive.f. Verify: gate-aot-positive; tools/engine-size.f; fixpoint. Depends: none (e74437cf landed). Ownership: AOT payload format. Claim: unassigned.

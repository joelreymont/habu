---
title: Name the engine-internal capacity seals
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T09:02:58.029938+03:00"
---

Problem: three engine exits print nothing when an internal bound is crossed (capacity lane survey 2026-09-18): src/habu/habu1.f:1451 (the DICT-CAP arm folded into a shared seal exit, rc 83), habu1.f:2585 (the NDICT / CODE-SPAN:RAW-MAX arms in the same shared exit, rc 83), src/habu/habu2.f:4533 (the live bind index against the certified LOWER-CERT:BIND-COUNT-CELL, rc 76); all engine-internal (no user program reaches them without an engine defect), which is why the capacity lane left them. Acceptance: each arm split out of its shared exit and named with the count and the ceiling in one stderr line through the LDIAGU helper the capacity lane adds, so an engine defect that crosses one reports what it crossed; a regression only where a fixture can reach the bound without a defect (else the change is verified by inspection and the fixpoint). Files: src/habu/habu1.f, src/habu/habu2.f, bootstrap/cg/forth.fs mirrors. Verify: fixpoint; bootstrap check; test/run.f. Depends: habu-name-silent-engine-9b28ac13 landing (LDIAGU). Ownership: engine. Claim: unassigned.

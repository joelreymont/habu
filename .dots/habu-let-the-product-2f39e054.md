---
title: Let the product engine compile the type-family merge at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T02:57:55.419270+03:00"
---

Problem: on the stdin/AOT-seeded product engine, `bin/hb --load test/native-fixture-write.f -- <out>` dies `ncomp: cannot compile REG-INCOMING? at TFAM:REG-AOT-MERGE-INCOMING?`, `hb: uncaught throw code -8286`, and succeeds on a native-runtime engine (double-load lane, 2026-09-17; the private-words lane saw the same on install-class engines earlier). It is the optimizing tier on src/core/type-family.f, not the debugger, and it means no cold-host fixture (every aot-* suite and test/cold-argv-separator.f) can run on a product engine, so the gate must run on the native-runtime lineage until it is fixed. Acceptance: the cause is named (which record or signature the product lacks for REG-INCOMING?, or which tier-1 path refuses it), fixed in the responsible layer, and test/native-fixture-write.f runs on the product engine; a fixture compiles TFAM:REG-AOT-MERGE-INCOMING? at tier 1 on a product engine. Files: src/core/type-family.f, src/compiler/native/*, src/habu/aot-capture.f, test/. Verify: the fixture on hb-fx3-class engines; test/run.f. Depends: none. Ownership: tier-1 on the product lineage. Claim: unassigned.

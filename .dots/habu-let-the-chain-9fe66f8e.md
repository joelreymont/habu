---
title: Let the chain tool copy the entry engine instead of moving it
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T09:43:28.051549+03:00"
---

Problem: tools/two-generation-build.f moves bin/hb to build/twogen/hb-entry for the chain and puts it back only on its own exit paths; on 2026-09-13 a completed run on the root (gen 5 = gen 4, rc 0 as far as the log shows) left bin/hb absent and build/twogen without hb-entry, and on 2026-09-12 a run killed mid-chain left bin/ empty (allocator lane). A root workspace whose engine vanishes breaks every concurrent suite child that spawns ./bin/hb (Maki and Tender both hit E-PROC-SPAWN this way before pinning) and forces a cold rebuild. Acceptance: the tool never moves bin/hb; it copies the entry engine into its private build/twogen directory (HB_TMP-derived), builds every generation from copies, and bin/hb is byte-identical before and after any exit path including a kill (a regression runs the tool against a copy in a scratch tree and asserts bin/hb's sha unchanged, and a second run interrupts it and asserts the same); docs/bootstrap.md's Generation Chain Check no longer warns about the moved engine. Files: tools/two-generation-build.f, docs/bootstrap.md, test/. Verify: the regression, the chain end to end. Depends: none. Ownership: hazel. Claim: unassigned.

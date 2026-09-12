---
title: Rebuild the node-intern index after boot and keep it across growth
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T12:48:02.932601+03:00"
---

Problem: the checker's node-intern table (UIX) is empty in a baked image by design (UIX-RESET: a baked image must carry no address for it) and UIX-SYNC drops the whole table when USIGS-GROW moves the store base, although entries are store offsets and the grow copies content verbatim; on the product engine the first user definitions re-write boot-prefix shapes and write them a third time after the 2 MiB cap, so tools/effect-store-census.f's identity NODES = SHAPES fails (test/effect-store-census-test.f F25, 1717 vs 1690; measured by the strict lane 2026-09-12 with a census probe: 27 duplicates, two clusters of the same 14 boot shapes). Acceptance: the intern index survives a base change (offsets need no rebuild), and a snapshot-booted engine rebuilds its index from the persisted store before the first intern (or persists it as offsets), so interning on the product equals the host's; the census identity holds on bin/hb and the suite is green; a regression that interns a boot-prefix shape after boot and after a forced grow and finds no duplicate node. Files: src/core/checker.f (UIX-*, USIGS-GROW), test/effect-store-census-test.f. Verify: the census suite, test/engine-suite.f, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.

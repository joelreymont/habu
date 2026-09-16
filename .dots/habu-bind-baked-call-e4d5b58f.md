---
title: Bind baked call sites by record at build time
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-15T18:52:59.705701+03:00\""
---

Problem: the AOT seed resolves every cross-word call in the baked code by NAME at every start (habu2.f EM-AOT-PATCH-SITES: 129,729 lookups on the pinned engine, each a linear scan because the hash index is built after the seed in EM-STARTUP), so a trivial program costs 1.38 s before its first token; the AOT name pool exists only for this. Acceptance: (a) the hash index is built before EM-SEED-AOT and the seed's records are indexed as they register; (b) sites are bound to record indices at build time and relocated arithmetically at start, no name lookup, the AOT name pool holds only what boot-run entry words need; startup of a trivial program under 20 ms. Files: src/habu/habu2.f (EM-STARTUP, EM-AOT-PATCH-SITES, EM-AOT-REGISTER-RECS), src/habu/aot-capture.f (site rows), bootstrap/cg/forth.fs mirror. Verify: perf record of a trivial run shows no scan; time bin/hb --load t.f. Depends: none. Ownership: engine boot. Claim: agent=hazel-private-words workspace=.jj-ws/hazel-private-words.jj-ws/hazel-recovery-run.
Evidence 2026-09-16 (docs/engine-size.md): part (a), the hash index before the seed, is already in the shipped engine; a trivial program starts in 28.4 ms median and the by-name site lookups are about 6 percent of that (1.7 ms), so part (b) buys 1-2 ms, 49,832 bytes of site rows and 656 bytes of names, not "most of the start". Every one of the 12,458 baked sites names a seeded primitive in wid 0 (83 distinct callees), never a captured word, so binding is: resolve the name against the primitive table at emit time, store the record index, boot patch = dict[k][0]. Do it as the small win it is; the start time itself is elsewhere (blob copy byte at a time over 1.89 MB, 15,641 record expansions, 257,959 DATA runs replayed, the 9 MB DATA zero loop at 3.7 percent).
Parked 2026-09-16 17:12 with habu-ship-no-dictionary-2fee2dea (same workspace .jj-ws/hazel-private-words, same WIP @ f2147916).

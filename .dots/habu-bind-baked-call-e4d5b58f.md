---
title: Bind baked call sites by record at build time
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-15T18:52:59.705701+03:00\""
---

Problem: the AOT seed resolves every cross-word call in the baked code by NAME at every start (habu2.f EM-AOT-PATCH-SITES: 129,729 lookups on the pinned engine, each a linear scan because the hash index is built after the seed in EM-STARTUP), so a trivial program costs 1.38 s before its first token; the AOT name pool exists only for this. Acceptance: (a) the hash index is built before EM-SEED-AOT and the seed's records are indexed as they register; (b) sites are bound to record indices at build time and relocated arithmetically at start, no name lookup, the AOT name pool holds only what boot-run entry words need; startup of a trivial program under 20 ms. Files: src/habu/habu2.f (EM-STARTUP, EM-AOT-PATCH-SITES, EM-AOT-REGISTER-RECS), src/habu/aot-capture.f (site rows), bootstrap/cg/forth.fs mirror. Verify: perf record of a trivial run shows no scan; time bin/hb --load t.f. Depends: none. Ownership: engine boot. Claim: agent=hazel-private-words workspace=.jj-ws/hazel-private-words.jj-ws/hazel-recovery-run.

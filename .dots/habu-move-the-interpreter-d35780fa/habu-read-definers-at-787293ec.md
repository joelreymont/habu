---
title: Read definers at tier 0 in the Habu loop
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T13:56:47.800320+03:00"
closed-at: "2026-10-02T14:04:49.252625+03:00"
close-reason: "done: def-open gives DKIND:VAL/DKIND:ADDR records the native tier, so the Habu loop reads create, variable and constant at tier 0; test/outer-interpret.f DEFINERS-TIER-0 (definers, JIT mentions, a tier-0 does> parent) agrees in both loops on spark gen2 == gen3 5e9280ac and fails on the base stack engine."
---

Problem: I7b (habu-compile-definer-bodies-1292d049) refuses create, variable and constant at tier 0 in the Habu loop (definers.f DEF-FIXED-HEAD -> DEF-TIER-0, rc 76 'hb: tier 0 is not in the Habu loop: <kw>'), written when the Habu loop had no tier 0 at all. I6 (habu-hook-tier-0-96e33c29) now runs tier 0 through jit-open/jit-token, so the refusal is the last tier-0 gap in the definers: the engine's loop reads them at both tiers. Acceptance: at tier 0 the Habu loop reads create, variable and constant as the engine's loop does (dictionary, DKIND, raw effects, check hook, does-patch), including words that tier-0 JIT bodies then mention; DEF-TIER-0 and DEF-RC-TIER-0 are gone; I7b's Habu-only tier-0 case in test/outer-interpret.f becomes a case both loops agree on; if NCOMP:COMPILE-FIXED cannot run at tier 0, fix the responsible layer rather than routing around it. Files: src/habu/definers.f, test/outer-interpret.f, plus the layer a reduction names. Verify: spark gen2==gen3, outer-interpret, engine-writers, does-clause-record, test/tier.f only if it already runs through the Habu loop.

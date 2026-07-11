---
title: "Maki lane: rewrite 26 finder call sites for the string-finder option migration"
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T18:23:29.847199+02:00"
---

Coordination dot FOR THE MAKI LANE (tfam is barred from maki/*). The wave-A string-finder finale (full plan + caller census in .dots/habu-switchover-wave-a-54edcee6.md) is blocked ONLY on maki-owned call sites: STR>NUMBER? callers maki/store.f:267 maki/cad.f:334 maki/golden-artifact.f:262 maki/saved.f:65; FIND-SUB callers in maki/lower-{ew,mm,red,mv}-test.f (13 sites) + maki/ablate-ptx.f; INDEX-OF callers maki/store.f (3) maki/cad.f (5) maki/golden-artifact.f (1). When the maki lane rewrites these to MATCH (per the plan's commit A/B/D shapes), the tfam lane completes: INDEX-OF + FIND-SUB + CONTAINS? -> option<idx>, STR>NUMBER? -> option<n>, deletes the temporary adapters STR>NUMBER-UNWRAP + STR>OPTION (lib/string.f) and the orphan tools/string.f. Until then the adapters are the documented boundary (behavior byte-identical, tested). tfam side is boundary-complete; coordinate the window via this dot.

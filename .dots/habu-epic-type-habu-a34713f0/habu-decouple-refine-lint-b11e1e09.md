---
title: Decouple refine-lint from the trust manifest
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T16:29:03.849528+02:00\""
---

Prerequisite discovered and precisely proven by the cast-migration landing (b019d98d): tools/refine-lint-core.f sources per-mint confinement metadata (owner liveness for STALE-SEED, allowed test references) from each mint's TRUSTED.md row, so migrating the 68 remaining eligible converters to CAST: (deleting their trust rows - the whole point of the retire-TRUSTED program) produces 40 STALE-SEED + 45 REFINE-CONFINE findings even though the migration passes trust-lint, trusted-inventory, the maki suite, and both cast suites. The confinement invariant (a raw-to-nominal forge stays module-private) is orthogonal to trust and must persist for CAST: mints. Fix: make the lint source-derived and CAST:-aware - (a) STALE-SEED liveness from the owner-source declaration (CAST: or TRUSTED:) instead of a manifest row; (b) allowed module-test references by the owner-stem-test.f convention plus the existing allow list. This deliberately changes STALE-SEED semantics and requires rewriting the RFLT-DRIFT-RED anti-rot test - a redesign of the confinement ratchet, hence its own reviewed change. After it lands, the 68-converter migration is mechanical (the cast lane validated the exact rewrite; script preserved in its scratchpad, regenerable). Red-first: the new lint must still fire on a genuinely unconfined mint and a genuinely dead seed under BOTH declarer forms.

Claim: agent=refinedec workspace=.jj-ws/fable-refinedec machine=spark (owns the source-derived CAST:-aware refine-lint redesign - the 68-converter prerequisite)

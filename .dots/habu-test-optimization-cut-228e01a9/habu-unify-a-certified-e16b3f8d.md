---
title: "Unify a certified word's effect with the top-level row"
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T22:03:23.449915+03:00"
---

docs/typed-top-level.md:92 (§2 table, certified word W): instantiate W's stored effect, unify its inputs against the top-level row, and on success set row := its outputs over the remainder, so output families propagate precisely; on failure warn at tier 1 and reject before the BLR at tier 2. Today src/core/top-row.f TR-WORD (:222-232 on 8bc7609c) models only set-check, xt and scalar consumers, +/- and the pure-consumer precise pop (TR-CERT-DOUT-EMPTY?/TR-CERT-STEP); every other certified word marks the row dirty; `rg 'E-INST|UNIFY' src/core/top-row.f` finds nothing. Comments at top-row.f:21 and :208 claimed this as closed dot 589c550f, which was the tier-2 switch (TR-TIER2?, top-row.f:109-112, landed). Acceptance: a certified word with typed outputs propagates its output families through the row (a top-level program seen failing first: a family mismatch after such a word is warned at tier 1 and rejected at tier 2), the existing top-row tests pass, docs/typed-top-level.md's row matches the code. Files: src/core/top-row.f (baked), test/top-row-*-test.f.

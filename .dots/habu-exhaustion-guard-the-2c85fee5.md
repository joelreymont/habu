---
title: Exhaustion-guard the existing RIGID-FRESH counter
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T22:58:57.244698+02:00\""
blocks:
  - habu-define-rigid-host-71b010a0
---

Loose end from the rigid host-region generations lane (2026-07-20): the existing shared RIGID-FRESH fresh-atom counter (src/core/checker.f:338, minted at E-I-AK checker.f:4329 and VREC-I-AK :2329 for fresh-extent-*/fresh-mask-*) has NO exhaustion-before-wrap - the mint is an unchecked 'RIGID-N @ dup 1+ RIGID-N !', so a wrap would silently reuse rigid ids and let unrelated instantiations unify. The new per-domain counters landing under habu-define-rigid-host-71b010a0 get E-RIGID-EXHAUST guards; retrofit the same guard to the legacy shared counter (or absorb it into the per-domain scheme if the landing makes it redundant - decide against the landed shape). Red-first: shrink the max under test, exhaust, catch the named throw; prove no wrap-reuse certifies. src/ change - exact-CODELEN Linux rows re-measured same-commit.

Claim: agent=rigidfresh workspace=.jj-ws/fable-rigidfresh machine=spark (owns the legacy RIGID-FRESH exhaustion-before-wrap retrofit: src/core/checker.f + rigid suite)

---
title: Census tier-1 passes across the Habu repos
status: active
priority: 1
issue-type: task
created-at: "2026-10-23T18:58:18.947695+03:00"
---

Instrument tier 1 (never landed) to count, per word, loops folded, spill slots and copies coalesced; run tools/tier-census.f at both tiers over every repo's entry files; report per repo and total: instructions, calls, frame traffic, movs, words with a fold or spill and their share, tier-1 word-size distribution, the 20 largest tier-0 minus tier-1 gaps. Lane: jerry, worker in .jj-ws.noindex/jerry-tm-census; results ~/.cache/tmp/jerry-tm-census/. Claim: jerry.

Partial result (2026-10-23, instrumentation WIP mwylksoo in jerry-tm-census; tier 0 -> tier 1): dots 2309 words, instructions 62675 -> 56013, bl 15209 -> 6925, sp traffic 11171 -> 8310, mov 71 -> 175; maki 15094 words, instructions 449803 -> 396395, bl 92574 -> 50154, sp traffic 85675 -> 67040, mov 101 -> 856. Loop folds: 0 in both. Words with frame slots: 27% (dots 671, maki 3964), holding 71.5% of tier-1 instructions; locals not yet split from pressure spills. Coalescing removes about 95% of copies. Tier-1 word size median 8-9, p90 62-67, p99 185-242, max 1271-1442 instructions. Not loadable on current habu: zzusage and blackboard (old `require lib/...` layout), kiba (missing kiba/src/lib/errors.f); etch.f through `required` dies E-EVAL-RESIDUE. Second worker continues (split spills from locals; tender, zen, loom, etch, habu src/lib).

---
title: Census tier-1 passes across the Habu repos
status: open
priority: 1
issue-type: task
created-at: "2026-10-23T18:58:18.947695+03:00"
---

Instrument tier 1 (never landed) to count, per word, loops folded, spill slots and copies coalesced; run tools/tier-census.f at both tiers over every repo's entry files; report per repo and total: instructions, calls, frame traffic, movs, words with a fold or spill and their share, tier-1 word-size distribution, the 20 largest tier-0 minus tier-1 gaps. Lane: jerry, worker in .jj-ws.noindex/jerry-tm-census; results ~/.cache/tmp/jerry-tm-census/. Claim: jerry.

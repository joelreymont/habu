---
title: Census tier-1 passes across the Habu repos
status: active
priority: 1
issue-type: task
created-at: "2026-10-23T18:58:18.947695+03:00"
---

Instrument tier 1 (never landed) to count, per word, loops folded, spill slots and copies coalesced; run tools/tier-census.f at both tiers over every repo's entry files; report per repo and total: instructions, calls, frame traffic, movs, words with a fold or spill and their share, tier-1 word-size distribution, the 20 largest tier-0 minus tier-1 gaps. Lane: jerry, worker in .jj-ws.noindex/jerry-tm-census; results ~/.cache/tmp/jerry-tm-census/. Claim: jerry.

Result (instrumentation WIP mwylksoo in jerry-tm-census, never lands; full table and maki top 20 in ~/.cache/tmp/jerry-tm-census/summary.txt). Words; tier-0 -> tier-1 instructions; share of words with a frame slot, with declared locals, with true spill slots (share of tier-1 instructions in brackets); coalesced/copies:

- dots 2309; 62675 -> 56013; frame 29.1% (71.5%), locals 35.2% (72.4%), spills 29.1% (71.5%); 3308/3487
- maki 15094; 449803 -> 396395; frame 26.3% (71.5%), locals 29.2% (71.4%), spills 26.3% (71.5%); 16821/17687
- tender 13994; 597833 -> 505454; frame 41.8% (83.7%), locals 45.8% (85.3%), spills 41.7% (83.7%); 38346/39600
- zen 10150; 352324 -> 295502; frame 36.2% (82.1%), locals 40.9% (84.2%), spills 36.2% (82.1%); 16812/17406
- zzusage 1204; 30336 -> 27236; frame 25.5% (70.2%), locals 32.0% (74.1%), spills 25.5% (70.2%); 1901/2037
- blackboard 1005; 25572 -> 23753; frame 24.6% (64.8%), locals 30.5% (69.6%), spills 24.6% (64.8%); 1122/1191
- kiba 1968; 45757 -> 39930; frame 28.4% (67.2%), locals 36.9% (73.8%), spills 28.3% (67.0%); 2099/2214
- habu src/lib (112 non-test files) 7113; 163667 -> 146621; frame 27.4% (67.8%), locals 34.2% (72.5%), spills 27.4% (67.7%); 7905/8371
- total of dots, maki, tender, zen and habu src/lib, each word once: 31646; 1194494 -> 1019050; frame 34.9% (79.2%), locals 38.2% (79.9%), spills 34.9% (79.2%); 61611/63964

Declared locals take no frame slot of their own: frame slots occur only with true spill slots (plus 5 words holding one padding slot). Of 11044 spilling words, 10870 declare locals and 173 do not; they hold 47861 spill slots against 43618 declared locals. Words with locals but no spill: 1215, 2.0% of tier-1 instructions. Loop folds: 0 in every corpus (a ?do probe counts 1 per loop, so the counter works). Tier-1 word size median 11, p90 85, p99 250, max 1464.

Not measured: etch (src/quantity.f `NEWTYPE length 0` passes the removed width argument, E-EVAL-RESIDUE on stock bin/hb too); loom (maki/infer requires the old `lib/...` layout). tools/tier-census-join.f throws E-JOIN-CAPACITY above 8192 words, so the top-20 gap table in summary.txt comes from direct pairing; its largest gaps are zen string-literal words (`EXTRACTION-PROMPT$` 1963 -> 6 instructions).

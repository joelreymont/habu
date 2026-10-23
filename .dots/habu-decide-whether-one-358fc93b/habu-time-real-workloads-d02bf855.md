---
title: Time real workloads at both tiers
status: closed
priority: 1
issue-type: task
created-at: "2026-10-23T18:58:18.959329+03:00"
closed-at: "2026-10-23T19:21:00.036695+03:00"
close-reason: measured; results recorded above
---

Run time and compile time at tier 0 and tier 1 on master's engine: zzusage over fixed JSONL input, dots over habu's .dots, etch, maki and tender heavy entries, tools/tier-bench.f; median of 3, load-only separated; then built-in profiles (top 15 words) of the three heaviest at both tiers. Lane: jerry, worker running from .jj-ws.noindex/jerry-tiermeasure; results ~/.cache/tmp/jerry-tm-run/. Claim: jerry.

Result (engine built at 387d46d0 in jerry-tiermeasure; median of 3, wall/user seconds; load = requires only, whole = load plus run, run = whole minus load; artifacts ~/.cache/tmp/jerry-tm-run/ times.tsv, raw/, prof/):

- zz daily --json, 230 JSONL rows: t0 load 0.16, whole 3.53; t1 load 3.53, whole 4.52; run 3.37 -> 0.99 (3.4x)
- dots ls x300: t0 load 0.32, whole 6.91; t1 load 8.20, whole 13.44; run 6.59 -> 5.24 (1.26x)
- dots tree x300: t0 load 0.34, whole 11.28; t1 load 7.58, whole 15.61; run 10.94 -> 8.03 (1.36x)
- tools/tier-bench.f: t0 load 0.06, whole 0.56; t1 load 0.79, whole 0.99; run 0.50 -> 0.20 (2.5x)
- tender test/c2-office-scale.f: t0 load 1.88, whole 65.18; t1 load 44.27, whole 82.66; run 63.3 -> 38.4 (1.65x)

Tier 1 runs faster everywhere but loads 11-23x slower; whole-program time is lower at tier 0 for every row except tier-bench's sub-second total. Profiles (`1000 prof-rate`, run phase): dots spends about 70% of samples in syscalls at both tiers (`write` via DOT-IO:PRINT, `open-rd`, `lstat64`), so the tier barely matters there; tender's time is in its document store (DOC:FIND-DOCUMENT per fact inside DOC:F-NEXT / DOC:FACT-OWNER) at both tiers, with tier 1 absorbing the primitives (`@`, `+`, `swap`, `=`, `cells`) that lead tier 0's profile. Not timed: etch (does not load, see the census dot), maki tests.f (rc 67 from its kernel worker; geometry runs in a separate OCCT process), repeated `dots find` in one process (hangs after the first call; one call takes 0.39 s).

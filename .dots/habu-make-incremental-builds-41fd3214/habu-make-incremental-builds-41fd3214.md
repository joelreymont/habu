---
title: Make incremental builds rebuild only what changed
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:16:47.852262+03:00"
---

Problem: measured on master 5b76e246 engine build with a persistent HABU_BUILD_CACHE: cold 92 s / 13944 compiled; no edit 0.74 s (artifact hit); one comment line in src/lib/memory.f 8.3 s / 656 compiled; one word in a die string in src/core/checker.f 39.5 s / 6675 compiled; an effect change plus one new word in memory.f 0 hits / 13945 compiled. Gate scripts used a fresh cache per run, so gates never reused anything. Review (2026-10-22, lanes A-D, findings in ~/.cache/tmp/carl-incr-review/) found the defects in the child dots; consolidated 2026-10-22 into six (generated words, storage, keys, drivers, gate row, test cruft) plus the kept reuse defects. Acceptance: every child closed; a one-file edit to the engine recompiles only that file's changed definitions and their real dependents, proved by a gate row. Claim: unassigned.

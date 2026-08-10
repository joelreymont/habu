---
title: Make load consult the registry it writes
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T19:23:58.204027+02:00"
---

bin/hb --load appends 's" path" provided' for each argv file and then inlines the text UNCONDITIONALLY (C-SOURCE-APPEND-ARG, habu2.f) - the marker exists so a later require no-ops, but the read itself never asks the registry, so preloading any path makes '--load <that path>' die duplicate-definition while 'require <same path>' no-ops cleanly (seeda lane 2026-08-11, reproduced both ways; 13 .f files fork bin/hb with a stdlib path on argv, 18 shell/doc lines). Fix: the argv row emits 's" path" required' - the loader consults the registry it maintains. KNOWN CONSEQUENCES to derive and pin, not discover: nested evaluate per file replaces one concatenated stream (diagnostics on uncaught throw change shape - pin the new shape); INCLUDE-BUF-CAP () applies per file instead of the 4MB arena (verify no --load consumer passes a bigger single file; the fixpoint command's largest is measured well under); tools/build-fixpoint-test.f SOURCE-BOUNDARY measures IBUFSZ through the argv path and must be re-derived WITH the change. BLOCKS habu-seed-the-stdlib-d8e3a757 (Stage A cannot land while seeded paths on argv are fatal). Files: src/habu/habu2.f, tools/build-fixpoint-test.f. Depends: none.

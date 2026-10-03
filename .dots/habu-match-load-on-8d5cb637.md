---
title: Match --load on names the engine cannot hold
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:18:22.291812+03:00"
---

check.f disagrees with --load on a first definition whose name the engine cannot hold. variable with an 8000-byte name: --load exits 71 ('definition body text full'), check.f 67 (uncaught 7143, E-TRUST-UNRESOLVED; 70 under --all-errors). A colon definition with a 7993-byte name: --load 71, check.f 70 (E-STATEMENT-THROW 7118). PRODUCT with an 8000-byte name: --load 67 (uncaught 7107), check.f 70. Repro: printf 'variable %s\n' "$(printf 'Z%.0s' $(seq 8000))" > v8000.f. The scan should refuse a name the engine cannot hold the way the engine does. Found while landing 1ca23983.

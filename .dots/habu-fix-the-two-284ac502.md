---
title: Fix the two engine-suite snapshot owner reds
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-12T00:51:04.796426+03:00\""
---

Problem: test/engine-suite.f is red on the root with two failures, F303 'hidx snapshot reset binds the live effect owner' and F308 'checker snapshot prepare rebinds persisted effect owner' (renumbered F304/F309 after the grain-cap case landed); pre-existing on 4f320647 and unchanged by the grain-cap change. test/run.f never reaches the engine suite because its pool aborts after the ten known reds, so nothing in the gate reports these. Acceptance: both cases green on the engine built from the tree, with the root cause fixed at the snapshot owner binding (not the assertions), and the engine suite reachable from test/run.f (the pool's abort-after-red rule stated and either fixed or the engine suite moved ahead of it). Files: src/core/checker.f (snapshot prepare and reset), src/habu/snap-lib.f, test/engine-suite.f, test/run.f. Verify: bin/hb --load test/engine-suite.f; test/run.f. Depends: none. Ownership: rowan. Claim: agent=hazel-worker workspace=.jj-ws/habu-fix-the-two-284ac502

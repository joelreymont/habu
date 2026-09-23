---
title: Key the hb-build object cache by the building engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-23T10:05:20.885821+03:00"
---

Problem: the hb-build object cache key already folds the building engine: HBB-MAKER-KEY! (tools/hb-build-lib.f:514-521) hashes BF-ENGINE$ together with the load, target and driver sources, so a build under a different engine misses and one under the same engine hits. No test pins that: lib/build-cache-test.f has no two-engine row, and the fetch-check lane's byte-identical hits across three engines (~/.cache/tmp/hazel-fetch-check/ timing logs) were read as a stale-key defect before the key was read. Once a checker or compiler change reinterprets what an image carries (the catch-stale lane repurposes the persisted effect fields EN.G/H as masks) a wrong hit would be wrong, not merely slow, so the invariant needs its row. Acceptance: a regression row in lib/build-cache-test.f builds one source under two engines (the lane engine and a second engine that differs - a lane fixpoint against the release engine) and asserts the second build is a miss and the third, under the first engine again, a hit; no change to the key. Files: lib/build-cache-test.f. Verify: bin/hb --load lib/build-cache-test.f tools/hb-build-test.f. Depends: none. Ownership: lib/build-cache-test.f. Claim: unassigned.

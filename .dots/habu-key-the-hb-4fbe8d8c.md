---
title: Key the hb-build object cache by the building engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-23T10:05:20.885821+03:00"
---

Problem: the hb-build object cache ($HOME/.cache/habu-build, lib/build-cache.f and tools/hb-build-lib.f) keys an object by source and options only: three different engines returned byte-identical images as cache hits during the fetch-check lane (evidence ~/.cache/tmp/hazel-fetch-check/ timing-before/after logs), and a fresh HOME per build was the workaround. Once a checker or compiler change reinterprets what an image carries (the catch-stale lane repurposes the persisted effect fields EN.G/H as masks) a stale hit is wrong, not merely slow. Acceptance: the cache key includes the identity of the building engine (its sha256 or the engine hash the image already records); a build under a different engine misses and rebuilds, a build under the same engine still hits; a regression row in lib/build-cache-test.f builds one source under two engines (the lane engine and a second engine that differs - a lane fixpoint against the release engine) and asserts the second build is a miss and the third, under the first engine again, a hit. Files: lib/build-cache.f, tools/hb-build-lib.f, lib/build-cache-test.f. Verify: bin/hb --load tools/hb-build-test.f lib/build-cache-test.f lib/codesign-test.f tools/hb-build-direct-lints-test.f. Depends: none. Ownership: lib/build-cache.f tools/hb-build-lib.f. Claim: unassigned.

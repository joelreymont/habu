---
title: Include prelude in build cache key
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T16:45:49.776100+02:00"
---

tools/hb-build-lib.f builds the maker cache key from HBB-KEY-LOAD-FILES (lines 472-503), a hand-maintained list of 30 source paths, together with HBB-KEY-COMMON-SOURCES, HBB-KEY-TARGET-SOURCES and HBB-KEY-DRIVER-SOURCES, all folded into the key by HBB-MAKER-KEY!. lib/prelude.f appears nowhere in that file, so editing the prelude does not change the key and a cached maker binary is reused unchanged.

Measured on master ed3465d3, and this corrects the original report: the omission is currently LATENT, not live. No file in the key list requires lib/prelude.f, and none calls a prelude-defined word (true, false, 0<>, fdrop, fdup, fover, f<=, f>=) in executable code. The single "true" in lib/sort.f is prose inside the header comment on line 4, not a call, so the claim that lib/sort.f now requires the prelude does not hold on master. The only other apparent hits across the keyed files are comment tails or string literals: lib/memory.f:252, lib/json-write.f:180, tools/source-discovery.f:132, tools/hb-build-report.f:99.

The defect is therefore structural rather than a live stale-binary bug today: the cache key is a hand-copied duplicate of the real load set, with nothing that keeps the two in agreement. This is the manifest-drift class, build-cache instance. As soon as any keyed file starts requiring lib/prelude.f, or any other unlisted file, stale binary reuse becomes silent and the resulting failure surfaces far from its cause.

Owned result: derive the key file list from the actual require graph, or add a parity check that fails when a file reachable from the maker's real load set is missing from the key list. Owner: tools/hb-build-lib.f. Forbidden: hand-adding lib/prelude.f to the list and calling it fixed, which patches one instance and leaves the drift mechanism in place.

Acceptance: (1) with lib/prelude.f genuinely reachable from a keyed file, editing lib/prelude.f provably changes the key produced by HBB-MAKER-KEY! and invalidates the cached build, demonstrated through the real bin/hb build path rather than a copied key model; (2) the drift check reds when a reachable file is omitted from the key list and greens when the list is complete, with hostile fixtures proving that a commented mention, a string literal, and a duplicate path do not count as coverage; (3) both diff lints pass.

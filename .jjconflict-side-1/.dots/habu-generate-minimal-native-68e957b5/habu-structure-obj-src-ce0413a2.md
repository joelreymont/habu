---
title: Structure object source identity
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:39:02.336476+02:00"
blocks:
  - habu-type-dsl-prove-93da83c4
  - habu-add-immutable-lexical-28b79e06
---

Invariant: an object cache source identity is one typed value with four named components: source content, target ABI, checker ABI, and compiler ABI. The current key, header-check, load, and index paths pass four indistinguishable pointer and length pairs, or twelve raw stack cells. Any permutation certifies but hashes or validates the wrong identity, and every caller repeats the same deep binder.

Define distinct package-owned span roles for the four components and one STRUCTURE source-identity value. Construct it once from validated object metadata and pass it through key generation, index storage, header validation, cache lookup, and load. Remove the raw four-span APIs and repeated high-arity binders; do not impose an arbitrary numeric argument limit. The global rule is semantic: recurring aggregates become typed values, while genuinely independent short-lived inputs remain ordinary stack arguments.

Dependency review 2026-07-21: use the unified STRUCTURE surface and the common immutable bounded-span capability. This leaf may define its domain roles, but it must not reproduce generic span ownership, bounds, or lifetime machinery.

Preserve exact object-source-index version-one key and header bytes, cache hits and misses, schema diagnostics, object contents, allocation bounds, and error propagation. Prove every pairwise field swap fails checking, malformed or wrong headers reject, exact key and header goldens remain byte-identical, cache and index round trips work across reload, stale content misses, bounds and canaries hold, and object, package, snapshot, ahead-of-time, fixpoint, and full native gates pass. Measure source tokens, definitions, dictionary names, JIT, DATA, CODELEN, key latency, and cache-load latency before and after; require shallower interfaces and no unexplained growth.

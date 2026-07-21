---
title: Move tests into test tree
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:40:31.868268+02:00"
blocks:
  - habu-own-legacy-test-db9327c6
  - habu-pkg-test-runner-bb5bef10
---

Invariant: test location mirrors production ownership under one canonical test root; production directories do not mix implementation and test modules. The repository currently has 454 test-shaped Forth files, 419 outside the singular test root. This scatters suite discovery, package ownership, cache identities, manifests, and direct-load commands across incompatible path conventions.

Define one canonical path mapping from a production module or subsystem to its mirrored test location. Move the corpus in small disjoint directory leaves, preserving exact suite order and one logical history per file. Update every require, suite registration, standalone-load route, manifest, generated source closure, cache key, temporary-root identity, documentation command, file map, and coverage inventory atomically with each leaf. Do not leave compatibility copies or path fallbacks. Document narrowly justified exceptions only for tests that must physically accompany external or generated assets.

Dependency review 2026-07-21: establish fixture package ownership and the reusable runner package before changing paths. The move must not combine ownership or public-API migrations with relocation, and it must not move the same files twice.

Add a checked path lint rejecting new test-shaped Forth modules outside the canonical tree and validate collision, case, duplicate-basename, direct-load, dependency-cycle, and stale-path failures. Prove every moved test still runs standalone and through its owning suite, every canonical inventory member runs exactly once, cache invalidation reflects the new path, and package, suite-coverage, host, file-map, bootstrap, fixpoint, Maki, PTX standard library, and full native gates pass on each leaf and the integrated tree. Measure source-path duplication, manifest rows, discovery time, and gate wall time before and after; require no unexplained growth.

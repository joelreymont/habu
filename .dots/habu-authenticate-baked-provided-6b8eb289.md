---
title: Authenticate baked provided manifest
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T21:56:14.355500+02:00"
blocks:
  - habu-checker-reject-compile-c8805039
---

Full context: modular composer destruction review found its require registry starts empty while native/recovery images already contain prefix files such as src/core/sha256.f. src/core/include.f currently exposes only mutable mixed REQUIRE-N/PATH state, and src/habu/habu2.f plus bootstrap/cg/forth.fs emit bare provided rows without an authenticated prefix boundary. Fix: add an immutable prefix-provided manifest capture API owned by the loader registry, with explicit begin/seal lifecycle during native and recovery image startup, exact canonical path ordering, digest/version identity, snapshot/AOT persistence, and read-only enumeration for SOURCE-COMPOSE. Core-prelude words may remain unqualified only because package syntax is not available at that boot stage; all later public APIs use a package and short uppercase tails. Reject mutation after seal, duplicate/noncanonical paths, absent/unsealed manifests, count/span corruption, and native/recovery identity drift. Acceptance: native and Gforth recovery produce byte-identical manifests/digests; src/core/sha256.f is present once; user provided/required/included events cannot pollute the prefix boundary; snapshot restore preserves identity; source composer seeds only from the sealed manifest and a real transitive require skips duplicate prefix definitions. Dependencies: checker loader lane owns src/habu/habu2.f and bootstrap/cg/forth.fs now; do not implement until it lands. Files: src/core/include.f, src/habu/habu2.f, bootstrap/cg/forth.fs, focused include/bootstrap/snapshot tests, then SOURCE-COMPOSE consumer.

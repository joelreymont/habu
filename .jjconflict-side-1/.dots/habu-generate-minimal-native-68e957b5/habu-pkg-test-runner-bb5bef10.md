---
title: Package test runner
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:38:05.823668+02:00"
blocks:
  - habu-pkg-process-core-2a6d3748
---

Invariant: reusable test execution is a library subsystem with one package owner; its focused tests own their fixture state separately. The current runner publishes the complete GT-prefixed implementation globally, and its focused test publishes GTT-prefixed buffers and helpers globally. Prefixes prevent some spelling collisions but provide no privacy and add dictionary bytes to every load.

Reopen TEST, shorten private implementation tails, and expose only the consumer operations required to register, launch, collect, and render a run. Put focused fixture state and helpers in a private fixture package with no reusable public API, use qualified calls or a lexically bounded using TEST block, and delete GT and GTT compatibility globals. Preserve command construction, environment and temporary-root handling, timeout and retry behavior, output parsing, ordering, errors, and exact rendered verdicts. Keep the runner implementation in its own file and responsibility while reopening package TEST; TEST is the sole package and namespace for test execution and assertions.

Prove old globals and private calls reject, public imports are collision-safe and scope-bounded, standalone and co-loaded tests retain exact behavior, and runner, process, standard-library, suite, package, host, and full native gates pass. Measure definitions, public names, dictionary-name bytes, JIT, DATA, CODELEN, and runner wall time before and after; require a smaller exposed and loaded surface with no unexplained growth.

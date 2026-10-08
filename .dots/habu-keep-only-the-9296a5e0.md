---
title: Keep only the Gforth bootstrap and the snapshot build
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.876646+02:00"
blocks:
  - habu-build-hb-once-feb85fd5
---

Problem: there are two ways to make an engine binary: the snapshot build (tools/native-build.f -> tools/native-build-core.f, 750 lines: load the sources into the running engine, capture its memory) and a glued-text compile (tools/build-fixpoint.f, 2,253 lines plus about 1,700 lines of its own tests: assemble prefix-src and stage2-src, certify, compile, iterate to a byte fixpoint). The Gforth bootstrap compiles stage2-src from zero.
Ruling (Joel, 2026-10-08): keep only the Gforth bootstrap and the snapshot build.
Acceptance: build-fixpoint keeps only what the Gforth bootstrap and the occasional census need; its refresh/install path and any duplicate of the snapshot build are deleted with their tests; the snapshot build stays the only way the product is made.
Files: tools/build-fixpoint*.f, tools/bootstrap.sh, tools/two-generation-build.f, docs/bootstrap.md.
Verify: snapshot build + full suite; Gforth bootstrap once.
Depends: the build-once test prelude dot. Ownership: unassigned. Claim: unassigned.

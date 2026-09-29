---
title: Select the build target in the capture window
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.723952+03:00"
blocks:
  - habu-bind-x86-host-4485acdd
---

Problem: `src/habu/native-runtime.f:35-49 PROVIDE-TARGET`, `tools/native-build-core.f:159-175 NB-TARGET-CORE-FILES` and `tools/build-fixpoint.f:847-929` select the seam from the running engine.
Acceptance: `tools/native-build.f -- <out> [whitebox] [--target linux-x86-64]` sets a build-target cell read by those three and by the manifest's backend rows; the window loads the x86 `target.f`/`layout.f`/`repl-term.f`; the host's own predicates are untouched outside the window; refuses a target whose backend module is not loaded.
Files: `src/habu/native-runtime.f`, `tools/native-build-core.f`, `tools/build-fixpoint.f`, `tools/native-build.f`.
Verify: spark: a `--target linux-x86-64` window loads the x86 seam files and refuses without the x86 backend module; a host-target rebuild is unchanged (chain); gate.
Depends: habu-bind-x86-host-4485acdd (K12).
Route: Alder (shared: src/habu/native-runtime.f, tools/native-build-core.f, tools/build-fixpoint.f, tools/native-build.f).
Ownership: krait (Intel lane).
Claim: unassigned.

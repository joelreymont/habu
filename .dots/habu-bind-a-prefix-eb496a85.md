---
title: "Bind a prefix word's call site in a baked image"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T21:08:32.441718+03:00"
---

Problem: the no-binary recovery chain now builds and boots hb-stage0, hb-stage (fixpoint) and hb-stdin-mk, and stops at `aot: call site 165 names NULL$ in scope 0, which is neither a seeded primitive nor a captured record` (exit 72; prefix-fit lane 2026-09-16, reproduced on the unmodified parent tree once the arena overflow was out of the way, recipe in docs/bootstrap.md). NULL$ is src/os/env-base.f:71, a prefix word that the AOT-captured src/habu/repl.f:147,152 calls; src/habu/habu2.f EMIT-AOT-SITES can bind a site only to a seeded primitive index or a captured record, so a call from captured code into a prefix word has nothing to name. Acceptance: a baked call site that names a prefix word binds (the prefix word becomes a captured record, or the site is bound by name through the boot rows like a require row is), the chain reaches `bootstrap check OK`, and a fixture in test/ builds an image whose captured code calls a prefix word and runs it. Files: src/habu/habu2.f (EMIT-AOT-SITES, EM-AOT-PATCH-SITES), src/habu/aot-decl.f, src/habu/repl.f, tools/bootstrap.sh, test/. Verify: HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 HABU_TARGET=linux-aarch64 tools/bootstrap.sh; test/run.f. Depends: habu-fit-the-recovery-ac33f757 (landed). Ownership: AOT site binder. Claim: unassigned.

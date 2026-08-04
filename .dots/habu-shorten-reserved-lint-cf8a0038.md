---
title: Shorten reserved lint private names
status: open
priority: 2
issue-type: task
created-at: "2026-07-23T06:50:30.765494+02:00"
blocks:
  - habu-pkg-reserved-name-65e4395f
---

Need: after package RESERVED-NAME-LINT owns the core, its private RNL-* stems remain migration debt and obscure the package-local vocabulary. Dependency: habu-pkg-reserved-name-65e4395f must be landed; this cleanup is not on the shared-lexer critical path. Owned result: tools/reserved-name-lint-core.f and its focused test only. Rename private RNL-* state and helper definitions to short package-local tails, update only internal references, and keep the six public words RESET, FILE-AS, FILE, JSON!, OUT-FD!, and FINISH unchanged. Preserve one definition per concept; no aliases, new exports, behavior changes, or shared-lexer edits. Acceptance: no RNL-* definition or reference remains in the core, the public dictionary surface is unchanged, and all diagnostics and counters are byte-for-byte equivalent. Production proof: reserved-name-lint-test.f, an external private-name rejection, typed-local-diff-lint and package-diff-lint on the exact diff.

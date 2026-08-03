---
title: Mirror the governance cleanup onto proofs
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T23:56:58.236789+02:00"
---

Master's cleanup (commits incl. 878d8cdab1bf Delete administrative scaffolding, 36cbfd70618c Delete suite coverage scaffold, 659690b6f332 Retire stdlib manifest ledger, a756fe8cf7d5 Compose trust and runtime deletions, 5497d494d3ca Delete release inventory scaffold) deleted the ledger/inventory layer; proofs diverged before it and today's ratchet lane (114cbc1f) repaired and extended that dead layer instead. Delete on proofs, mirroring master: TRUSTED.md; tools/trusted-inventory.f + -test.f; tools/trust-lint.f + -core.f + -test.f; tools/primitive-effect-inventory.f + -test.f; tools/stdlib-manifest-test.f (and stdlib-manifest core files if present); tools/suite-coverage-lint.f + -core.f + -test.f; tools/host-lint.f + host-lint-test.f; tools/release-inventory.f; tools/landing-gate.f (added today, same class). Deregister their SUITEs from test/gate-stdlib-cases.f and the fork groups; remove references from kept files — where master still carries a kept file, master's version is the guide; rewrite docs/forth.md's Commit gate section without the pin ceremony (behavior suites + maki + the two diff lints + error-code-lint + dot lint). KEEP: text-foundation-test, examples-test, error-code-lint, package-diff-lint, typed-local-diff-lint, dot-dep-lint, checked-boundary-lint (all kept on master), the formal insn/reloc parity gates (proofs-campaign property, not a ledger — reloc-proof stays), STATUS.md (campaign state, decide separately), and every behavior fix from today's lane (LINT-SLAB runtime sizing, EXT-COPY-ENSURE, the PS packaging, COND-FILE$ repoint). Close moot ledger dots (habu-rebind-trust-rows-e34cee54, habu-discharge-the-native-7666552f, and any other whose only substance is TRUSTED.md rows). Gates after: gate-stdlib minus the deleted suites green, maki 200/200, native suites, codegen-compare, the kept lints. CLAUDE.md still names host-lint in the master-green rule — flag for the user, do not edit CLAUDE.md.

Claim: agent=governance-mirror workspace=.jj-ws/habu-mirror-the-governance-9dcdbf3a

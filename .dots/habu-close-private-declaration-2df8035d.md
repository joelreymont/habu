---
title: Close private declaration-event tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T20:49:38.708731+02:00"
blocks:
  - habu-migrate-payload-authz-e013ef3e
---

Why: after the public and mutation replacements land, test/decl-event-suite.f must no longer publish into or inspect the production DECL-EVENT owner. Owner: test/decl-event-suite.f and its structural test only. Exact result: remove the package DECL-EVENT test block and every remaining private definition or access while retaining all migrated public test packages and the existing report. Acceptance: a token-aware structural check finds no package DECL-EVENT, direct set-current to either owner wordlist, unqualified private definition, DEV identifier, or DEVTX access in the suite; declaration-event, type-family, generated-declaration, ENUM, and STRUCTURE suites remain green; preceding mutation evidence remains discriminating; test count changes only where a private intermediate assertion was explicitly replaced by its public contract. Forbidden: allowlist, path exemption, trusted replacement, commented-out tests, substring-only proof, or coverage deletion. Smallest check: structural test plus declaration-event suite. Depends: Migrate payload authorization tests. Claim: unassigned.

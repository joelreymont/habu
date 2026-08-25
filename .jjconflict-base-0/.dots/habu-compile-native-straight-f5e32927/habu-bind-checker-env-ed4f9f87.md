---
title: Bind checker environment
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:02.995534+02:00"
blocks:
  - habu-capture-compiler-src-01c8f962
---

Full context: design sections 7.1, 7.2, 16.2, and cache/proof contracts require a frozen checker/environment manifest plus compiler/checker identities bound to the source tape and every artifact. Acceptance: source/checker/environment/compiler digest mismatches reject before HIR publication; manifests encode canonically and change for every semantic environment change. Dependency: source tape.

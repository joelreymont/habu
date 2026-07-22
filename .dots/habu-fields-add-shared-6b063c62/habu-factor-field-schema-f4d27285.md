---
title: Factor field schema validator
status: active
priority: 2
issue-type: task
created-at: "2026-07-17T12:12:06.142339+02:00"
blocks:
  - habu-harden-field-tokens-e855dc36
---

Style review findings: PF-NODE-KIND? at src/core/type-family.f:945 is a roughly 60-line recursive dispatcher mixing PARAM/CON/PTR/QUOT/APP classification and validation, obscuring fail-closed branches; the shared PF slice also spells core built-ins such as IF, THEN, BEGIN, WHILE, EXIT, and RECURSE in uppercase, contrary to docs/forth.md. Fix: split each schema kind into a named uppercase checked helper with a real typed stack effect, isolate root-range and recursive-child validation, retain one shallow dispatcher, lowercase every core/system word touched by the PF implementation while keeping project-defined words uppercase, preserve exact results and error ownership, and add branch-focused negative fixtures. Acceptance: no giant or multi-concern word remains, no uppercase core/system word remains in the changed PF slice, typed-local-diff-lint and source-style gates are green, and type-family, declaration, fixpoint, and full gates remain green. Files: src/core/type-family.f shared PF implementation, test/type-family-suite.f.

Claim: agent=factor_field workspace=.jj-ws/habu-factor-field-schema-f4d27285

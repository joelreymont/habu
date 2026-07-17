---
title: Exhaust field reserved names
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T12:12:34.974497+02:00"
blocks:
  - habu-isolate-field-valid-16a9e2ac
---

Test review finding: PF-RESERVED?/TF-GRAMMAR-KEYWORD? at src/core/type-family.f:900 is a hand-maintained grammar/generated-tail table but declaration fixtures cover only field and make. Fix: define one canonical reserved-tail source or table-driven checked fixture and prove every lowercase grammar/generated tail rejects with E-PF-NAME; separately prove uppercase/mixed-case rejects at the canonical-name boundary with E-TFAM-CASE, so case behavior is not conflated. Include one adjacent nonreserved positive control and keep the test synchronized with the production table. Acceptance: removing any reserved row makes the focused suite red; declaration/type-family suites, typed-local lint, full gate green. Files: src/core/type-family.f:900-925 if a single-source table is needed, test/type-decl-suite.f, test/type-family-suite.f.

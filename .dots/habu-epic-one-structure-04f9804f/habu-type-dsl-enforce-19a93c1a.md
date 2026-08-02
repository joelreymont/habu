---
title: "Type DSL: enforce one-surface lint"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:47:00.375638+02:00"
blocks:
  - habu-type-dsl-delete-8bd73b41
  - habu-lint-gate-sole-5201b95d
---

Add Habu-native lint coverage that rejects removed type definers in live source and generated source while allowing only dedicated negative fixtures and historical prose where explicitly classified. Rewrite docs/forth.md, docs/type-families.md, MODEL-CAD-V2-PLAN.md, examples, and LLM-facing grammar/schema documentation around only STRUCTURE and ENUM. Add concise machine-readable declaration schemas and repair guidance. Gate namespace, reserved-name, signature, source-list, and generated-source outputs.

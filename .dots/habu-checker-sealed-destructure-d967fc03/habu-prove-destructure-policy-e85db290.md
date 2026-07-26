---
title: Prove destructure policy after rebuild
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T21:36:26.840068+02:00"
blocks:
  - habu-store-structure-destructure-8c20c92a
---

Problem: direct registry tests cannot prove that a type family destructure policy survives native image publication and reload. Required result: extend the existing authoritative AOT/fixpoint registry capture and restore path so the destructure-policy cell added by habu-store-structure-destructure-8c20c92a is serialized and restored as part of the TFAM record, without a parallel manifest, table, or default-on-load fallback. Add the smallest production observation seam needed to declare a PUBLIC family and an OWNER family before the real native rebuild, load the rebuilt image, and query the same family identities afterward. Owner: existing AOT/fixpoint registry publication and its production rebuild test only. Dependencies: habu-store-structure-destructure-8c20c92a. Acceptance: the real rebuilt image returns PUBLIC for the public family and OWNER for the owner-only family; corrupt, missing, or invalid policy bytes fail closed instead of becoming PUBLIC; existing registry and AOT image suites remain green. No syntax, lowering, generated-word, or package-visibility behavior belongs in this leaf.

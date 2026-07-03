---
title: Rename checker signature registries
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T18:06:00.604724+02:00"
---

Context: src/core/checker.f names USIGS for certified non-primitive word effects and PES for primitive-effect rows. Both names obscure the model: USIGS is now a certified word-effect registry, PES is the primitive/native effect registry. Later cleanup should rename USIGS/USIG-* to CERT-SIGS or WORD-EFFECTS and PES/PE-* to PRIM-EFFECTS, updating docs/tests after current cert RCA lands.

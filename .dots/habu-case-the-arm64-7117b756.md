---
title: "Case the arm64 layer's builtins to convention"
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T15:41:15.612675+02:00"
---

src/arch/arm64/asm.f and icode.f use upper-case IF...THEN builtins against docs/forth.md naming — pre-existing, whole-layer, mechanical. Normalize when the layer is next touched; do not run a standalone churn pass over files the hard cut may delete or rewrite (check the cut's file fate first).

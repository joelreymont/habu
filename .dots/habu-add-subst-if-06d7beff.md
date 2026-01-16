---
title: Add subst-if/subst-if-not primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:24.330933+02:00"
---

src/runtime/primitives/list.zig: Implement conditional subst
- subst-if: substitute where predicate true
- subst-if-not: substitute where predicate false
- nsubst-if, nsubst-if-not: destructive versions
- Support :key parameter
- Add tests for predicate-based substitution
- Est: 20 min

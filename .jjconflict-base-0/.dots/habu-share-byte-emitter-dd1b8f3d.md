---
title: Share byte-emitter arithmetic across IR renderers
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T03:22:39.137197+02:00"
---

Full context: from agent irrender 2026-07-30. Four files now carry small private byte writers with duplicated arithmetic: src/compiler/ir/type.f (FRENDER), attr.f (FRENDER), render.f, diff.f. The SINKS genuinely cannot merge (each writes into its own buffer discipline) but the shared part - ordinal/number-to-text arithmetic and span bookkeeping - should be value-style emitter words published by ONE owner (likely a small src/compiler/ir/emit-text.f package or an existing lib/ text module - measure which is the real owner) and consumed by all four. Adopting type.f and attr.f makes this its own leaf; keep the emitters value-style (no shared mutable sink) so the four buffer disciplines stay untouched. Gate: the four files' rendered goldens byte-identical before and after.

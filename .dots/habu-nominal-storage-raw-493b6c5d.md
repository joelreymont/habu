---
title: "Nominal storage: raw type-variable kinds"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T15:48:44.631633+02:00"
---

Phase 1 of habu-checker-seal-nominal-0b2eaece. Add TVK-ANY and TVK-RAW to checker effect variables; mark here, create, variable, constant, and verifier-created raw definer effects RAW; propagate through copy, freshening, unification, rollback, snapshot, native and bootstrap paths. RAW may unify with plain scalar representation but must reject nominal atoms, arity-zero families, linear/layout values, and structured pointers containing nominal state. Preserve generic numeric cells and zero per-fetch lookup. Acceptance: variable, create, constant, and here laundering negatives; numeric generic positives; stable qualified diagnostics; native fixpoint, bootstrap, engine and full gates.

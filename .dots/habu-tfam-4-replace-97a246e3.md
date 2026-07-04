---
title: "TFAM 4: replace PARAM-CTOR? + nested param parsing"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.926647+02:00\""
---

PLAN.md item 4. Package-aware TFAM lookup replaces whitelist (src/core/checker.f:1743-1773); register PTX cell families at core load; recursive-safe growable schema args replace PARAM-SCR global + PARAM-MAX-ARGS=4 cap; T-PARAM stores resolved family-id (spelling diagnostics-only); unification by family-id; SC-QUOT quotation payload schemas parse/persist/instantiate/render; VREC/effect replay preserves family-id (checker.f:1508,1584,2760,2937). Gate 17d. Depends: TFAM 2a, TFAM 3.

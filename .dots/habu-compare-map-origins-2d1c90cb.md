---
title: Compare map origins by record identity
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T08:56:24.166226+03:00"
---

Matching source-only recovery host c1c992d0 fails loading tools/check.f and native tool consumers in ENGINE-ORIGIN MAP-FIND: expected n n at =, actual typed ptr n and raw numeric dbase+idx*48. Exact declaration record identity is required because EXPORT aliases can share a code address. Compare like-typed dictionary record pointers using the existing XREF record API; retain alias and supersession behavior. Verify the previously failing real tool load and existing AOT map E2E with a matching native host, and obtain independent review before landing. This is a source-load blocker for incremental native build qualification.

The real native build later reaches MAP-TARGET during the checked prefix, before prelude supplies 0<>. It refuses that token at exit 74. Use seed-native 0= 0= for this nonzero predicate and audit the map helpers' actual early vocabulary; ordinary source-host loading with an already loaded prelude does not expose this ordering failure. Preserve exact record identity and source tombstones.

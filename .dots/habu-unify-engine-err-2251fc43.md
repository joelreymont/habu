---
title: Unify engine error namespace
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T03:02:16.293300+02:00\""
---

Full context: checker-current-wid assigns exit 86 to E-CALLABLE-ABI while catch-return independently assigns 86 to E-CATCH-STACK and 87 to E-CODE-CERT. Before integration, define one authoritative package-based ENGINE-ERROR ABI in src/habu/layout.f with recovery mirror, preserve existing 83-85 codes, allocate unique CALLABLE-ABI/CATCH-STACK/CODE-CERT codes, remove new global E-* aliases, and add native/recovery uniqueness and exact-exit tests. This is an integration dependency; do not patch lanes independently.

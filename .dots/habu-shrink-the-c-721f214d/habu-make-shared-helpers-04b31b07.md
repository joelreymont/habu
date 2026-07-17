---
title: Make shared helpers AOT-closed
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-17T14:59:52.866193+02:00\""
---

The primitive guard-sharing cut adds direct BL edges from AOT-copyable primitive records to LPROTSPAN, an internal engine routine outside every dictionary closure record. test/gate-aot-positive.f GAP-DATA fails closed with 'aot: PC-relative target removed or outside closure'. Add an inaccessible private engine-helper record for the shared guard and teach stripped-AOT closure discovery to include direct B/BL targets that resolve exactly to registered records. Preserve ordinary intra-word branches, relocation, package visibility, protection semantics, and compact AOT output. Acceptance: minimal direct-helper closure regression, GAP-DATA/GAP-BUNDLE, focused guard gates, full gate.

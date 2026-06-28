---
title: Symbol-only checker records
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T19:00:24.982952+02:00"
---

Problem: src/core/checker.f has symbol ids for package/name resolution, and primitive effects are now keyed by symbols, but certified user records still keep name bytes for USIG-NAME$/USIG-MATCH?, diagnostics, truncation, and some boundary lookups. Fix: make user signature/control/defer metadata primarily keyed by stable symbol ids; keep source bytes only in the symbol table for diagnostics. Acceptance: user lookup/truncation/hide/delete paths operate by symbol id; string comparison is confined to symbol interning/diagnostics; no regression in package visibility, undefine, CHECK-CANDIDATE!, defer/is, no-return metadata; add negative tests for duplicate/shadow/undefine across packages; rebuild bin/hb and run full native gate.

---
title: "Visibility discharge: 548 shims are not a typing problem"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.371299+02:00"
---

Phase 1 of habu-trusted-dies-prim-4fd12d60, the highest-leverage cut: 548 of 1,373 TRUSTED: sites (census 2026-08-19) are single-call shims onto words that are ALREADY checked : definitions - the checker knows their effects; internal-mark.f seals the name or the pre-hook prefix never recorded a usig at the call site (internal-mark.f:26 states it). Includes 79 of the 107 sites in the decl machinery (enum-decl 41/44, structure-decl 38/40, generated-declaration 11/23, structure-make 6/8) and ~395 of test/'s 826. Build ONE capability: publish an already-known signature to a named consumer package (friend-import / signature re-export). PROBE FIRST what internal-mark/prot-wid/friend arenas already provide - the capability may be an extension, not a new mechanism. Then sweep the 548 mechanically. Blocks the final TRUSTED: deletion.

---
title: "Switchover wave C: outcome sum + block ENUMs for in-process tags"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.009104+02:00"
---

docs/census-switchover.md sections 2+5 wave C. PROC-STATUS>OUTCOME (process.f:78 kind+code) becomes SUMTYPE outcome: exited<n> | signaled<n> | timeout; retire PROC-OUTCOME>RC sentinel folding (:86-90) where callers can take the sum. Block ENUMs for in-process tag clusters: MAP-EMPTY/DELETED/OCCUPIED (map.f:15-17), FDEF-N/PTR/NOM/VOID (ffi.f:13-16), JSON-PARSE-OK/THROW (json.f:104-105). NO persisted-value clusters (T-*/VR-*/SC-*/TK-*/TL-* stay — wave E decision). DEPENDS: items 9, 14.

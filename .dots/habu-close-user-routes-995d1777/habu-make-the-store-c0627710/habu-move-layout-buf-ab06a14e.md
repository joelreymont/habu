---
title: Move layout-buffer.f into package LBUF
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T21:41:24.108703+01:00"
---

Problem: src/core/layout-buffer.f defines its storage helpers as global words that call checker words (STORAGE-BIND and NODE-BIND pass a caller's handle and type to CHECKER-STORAGE-BIND); once the checker files go private, global words cannot reach them. Acceptance: every word of the file but its three error constants lives in package LBUF, public only where another file calls it; TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER and DEFER-LAYOUT-BUFFER stay callable bare; the engine builds and the storage suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-pub.

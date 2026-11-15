---
title: Place a pointer-to-linear rejection inside an application at its pointee
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:36:26.181882+01:00"
closed-at: "2026-11-16T04:07:35.048400+01:00"
---

Problem: src/core/structure-decl.f SD-PTR-NODE rejects `option<^edlmodel>` with the record at the whole type, where the top-level `^T` path places it at the pointee. Acceptance: the record stands at the pointee text, no other record moves, pinned by an enum-decl-suite case that also pins the composed 'payload type' label. Depends: none. Claim: agent=W173 workspace=.jj-ws.noindex/jerry-w173.

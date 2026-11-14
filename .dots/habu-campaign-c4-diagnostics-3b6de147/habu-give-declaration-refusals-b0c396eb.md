---
title: Give declaration rejections a place and a fix
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.365388+01:00"
---

Problem: src/core/generated-declaration.f GUARD-CODE and src/core/sumtype.f TDECL-RUN pick a reason from the EXPLAIN table with no fix text, print no line or column under --load, and print 'declaration failed' over an earlier diagnostic. Acceptance: each declaration rejection prints one diagnostic through REJECTION (src/core/rejection.f) with the declaration's place and a reason-specific help; a status already printed and a code with no reason row are rethrown, not reprinted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4d1a (D1a), jerry-c4d1b, jerry-c4d1c, jerry-c4d2a, jerry-c4d2b; integration .jj-ws.noindex/jerry-c4.

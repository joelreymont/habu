---
title: "Keep a DERIVE init rejection's token"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T11:44:28.043415+01:00"
---

Problem: `PRODUCT ckdini DERIVE init FIELD x n ;PRODUCT` is rejected `derive init requires a canonical fixed-cell record at '\0\0\0\0\0\0'`: the token printed is six NUL bytes, also on an engine where TDECL-RUN reports before its rollback (src/core/sumtype.f TDINIT-REFUSE about :1925). Acceptance: the rejection names its token; a type-decl test. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-blank.

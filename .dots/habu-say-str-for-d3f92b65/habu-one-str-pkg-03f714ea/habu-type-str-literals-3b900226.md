---
title: Type string literals as str
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T11:23:37.243415+01:00"
---

Problem: s" gives ^u8 n (src/core/checker.f:13601) and 30 primitive rows say ^u8 n for bytes they read or give, so unmarked files write >str after literals. Acceptance: literals, [package] and those rows give str; the unmarked files pass; docs say so; the code that carries a literal across >str is deleted in a second lane. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-strlit.

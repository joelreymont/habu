---
title: "Delete SOURCE-ROOT:LOAD"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:17:55.186428+01:00"
---

Problem: SOURCE-ROOT:LOAD (src/core/include.f) loads its path without resolving it, so it enters the root the last resolve left (none in `hb file.f`: error[E-PATH-RANGE] 'a path is empty', naming no word) and opens a relative path from the process directory. Its one caller is test/policy/allow-root-direct.f:10; zen, tender and maki call none. Acceptance: the word is gone, allow-root-direct.f loads through `included` with the same confinement record. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w225.

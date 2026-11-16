---
title: Render a refused EXPORT of a declaration
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:22:24.231594+01:00"
---

Problem: 'EXPORT typed-variable' inside a package prints only 'hb: uncaught throw code 7120' and exits 67: EXPORT-RESOLVE (src/core/checker.f:16031) throws E-EXPORT-UNSAFE without rendering a record or calling CHECKER-REFUSE (1795), so habu2.f:2344's exit takes the bare path. Acceptance: it prints a rejection with its place, the word and the fix, and exits 70 as a rendered rejection. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-pub.

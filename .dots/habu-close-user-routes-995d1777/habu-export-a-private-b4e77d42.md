---
title: Export a private word of an unsealed package
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T19:39:41.012628+01:00"
---

Problem: engine packages must call the checker's store writers once those are private to CHECK; EXPORT refuses a private source (E-EXPORT-UNDEFINED). Acceptance: EXPORT PKG:NAME takes a private NAME of an unsealed PKG in both the engine alias and the checker record; a sealed package's private word stays E-EXPORT-UNDEFINED; a test in test/export-package.f. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-store.

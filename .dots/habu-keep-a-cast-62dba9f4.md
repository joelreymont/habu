---
title: "Keep a cast signature's variable kinds through the walk"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T07:23:54.256411+01:00"
---

Problem: checking CAST: >QB ( n -- qb<t> ), where STRUCTURE qb<a> FIELD f [ -- a ] ;STRUCTURE, lowers t's kind from TVK-STORED (11) to TVK-CELL (9) during the atom walk of CAST-CERTIFY, somewhere in TFAM-MEMBERS -> SCHEMA-TERM-FOR (src/core/type-family.f:4259); measured with probes in the cast-3 lane. A check walk that rewrites the declared signature may change what the cast's saved row admits at its callers (qb<t> recorded as one cell instead of any stored type). Acceptance: the root cause named at its write; the walk leaves the signature's variables as declared, and a test shows a caller of a stored-variable cast bound to a two-cell type is checked as before the walk. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-kind

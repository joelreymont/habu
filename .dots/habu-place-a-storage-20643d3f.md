---
title: "Place a storage definer's name rejection at the name"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:17:16.556481+01:00"
---

Problem: 'TYPED-VARIABLE if n' is rejected E-RESERVED-DEFINITION at 1:19, the type token n, under --load and check.f: every child-making definer reaches src/habu/interpreter.f CHILD-RECORD, which passes the token being read, not the child's name, to DEF-RECORD as the place of every record it and DEF-WALL write. Acceptance: --load, check.f and --verify-only give the same record at the name token for a keyword or number name under every storage definer. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-sp.

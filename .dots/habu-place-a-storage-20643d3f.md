---
title: "Place a storage definer's name rejection at the name"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:17:16.556481+01:00"
---

Problem: under --load, 'TYPED-VARIABLE if n' is rejected E-RESERVED-DEFINITION at 1:19, the type token n, where check.f and --verify-only place it at 1:16, the name: src/core/layout-buffer.f TYPED-VARIABLE (~493) reads the name, then the type, and the name check (LBUF-NAME-OK?, CHECKER-LBUF-NAME-OK?) places its record at the current token; the other storage definers that read a type after the name (TYPED-BUFFER and the rest) likely share the path. Acceptance: --load, check.f and --verify-only give the same record at the name token for a keyword or number name under every storage definer. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-sp.

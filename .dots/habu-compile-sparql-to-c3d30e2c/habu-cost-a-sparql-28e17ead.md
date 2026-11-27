---
title: Cost a SPARQL constant only the bytes it holds
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T14:34:40.245688+01:00"
---

Problem: src/lib/sparql-test.f CONSTANTS (65,536 VALUES constants, 382 KB of query) maps 97 MB in 9 arena chunks before EMIT rejects it at PostgreSQL's 65,535 parameters: about 950 bytes per 6-byte constant, charged to SEG-ENSURE (segs.f:40), PARAMETER (emit.f:463), store.f's TERM-START, +TERM-MEMBER and NEW-WRITER, and iri.f JOIN$. Joel: a SPARQL compile works within 1 MiB. Acceptance: a VALUES constant costs a compile no more than its nodes, its parameter's text and its SQL; bytes per constant stated for a 4,096-constant VALUES before and after; every SPARQL test answers as before. Depends: none.

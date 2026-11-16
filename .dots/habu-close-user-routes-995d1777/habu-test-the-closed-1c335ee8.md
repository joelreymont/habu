---
title: Test the closed routes under both loaders
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T19:39:41.057670+01:00"
---

Problem: the parent dot's program, these and the census probes have no test: `: FOOB ( -- n ) 1 ;  s" FOOB" CHECK:PRIM-OPEN 8 CHECK:PRIM-ATOM 2 CHECK:PRIM-ATOM s" " true false CHECK:PRIM-CLOSE  : T2 ( -- n ) FOOB ;`, `s" T:R" CHECK:PRIM-OPEN s" " false false CHECK:PRIM-CLOSE : P6 ( -- ) ; CHECK:PRIMS-END` and `: FOOB ( -- n ) 1 ; s" FOOB" s" -- n n" TRUST : T ( -- n ) FOOB + ; T . cr` (each now undefined), and some tests call the writers directly. Acceptance: test/engine-private-test.f rejects each program as undefined under --load and tools/check.f; a test that called a private writer states its case as source or is deleted (test/AGENTS.md hand-built state). Depends: habu-make-the-store-c0627710. Claim: agent=carl workspace=.jj-ws.noindex/carl-pub.

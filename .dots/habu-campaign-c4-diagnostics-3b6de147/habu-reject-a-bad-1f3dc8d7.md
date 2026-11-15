---
title: Reject a bad FFI clause with a record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:54:27.389610+01:00"
closed-at: "2026-11-16T04:07:34.760548+01:00"
---

Problem: src/lib/ffi-abi.f REFUSE throws a bare E-FFI-SYNTAX for every clause condition (CLAUSE-TARGET, VARIADIC-CLAUSE, EXTENT-CLAUSE, LENGTH-CLAUSE, STRUCT-RESULT-CLAUSE and the rest). Acceptance: each condition prints one placed record at the clause word and stops with 70 under bin/hb. Depends: habu-write-a-placed-a652e82a. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

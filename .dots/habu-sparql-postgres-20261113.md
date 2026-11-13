---
title: Compile SPARQL in Habu and evaluate it in PostgreSQL
status: active
priority: 1
issue-type: task
---

Replace the Rust SPARQL bridge with Habu. PostgreSQL remains the store and
evaluates joins, paths and aggregates; Habu owns syntax, parameterized SQL
generation and standard result serialization. Models receive only SPARQL.

Public vocabulary: QUERY, a source callback and an output callback. The
source receives a SQL descriptor once, then next-batch requests. Its caller
owns authorization, the read-only snapshot, the cursor and the output bound.
An absolute deadline covers the Habu compiler and the host's database waits.

Acceptance: a native load; the public callback/serialization tests; the same
pinned W3C profile (316 syntax and 539 evaluation cases); Zen and Tender's
real consumer paths. Preserve the existing comparison policy. No external
Rust library or required SPARQL worker executable after migration.

The implementation is currently uncompiled source. The old Rust engine's
839/855 result does not qualify it. PostgreSQL projection/helper assertions
and an independent libpq cursor probe qualify their respective boundaries
only. The x86 T0 branch supplies the missing native compiler; full Habu and
MCP execution remain open until that compiler is runnable.

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

The parser, SQL compiler and public callback/serialization tests execute on
the recovered ARM T0 host. The W3C profile and production PostgreSQL queries
run through that compiler with the original result comparison. Native x86
qualification and Zen/Tender's HTTP/MCP consumers remain open; the recovered
host's evidence does not qualify the current x86 compiler.

---
title: Compile SPARQL in Habu and evaluate it in PostgreSQL
status: active
priority: 1
issue-type: task
---

Replace the Rust SPARQL bridge with Habu. PostgreSQL remains the store and
evaluates joins, paths and aggregates; Habu owns syntax, parameterized SQL
generation and standard result serialization. Models receive only SPARQL.

QUERY-ROWS passes the host a scoped SQL statement and parameter iterator.
The host feeds borrowed database rows to WRITE-ROW. QUERY supports a byte
transport with a statement descriptor and subsequent batch requests. Both
share the parser and result writer. The host owns authorization, snapshot,
cursor and output budget. One deadline covers compilation and database waits.

Acceptance: a native load; the public callback/serialization tests; the same
pinned W3C profile (316 syntax and 539 evaluation cases); Zen and Tender's
real consumer paths. Preserve the existing comparison policy. No external
Rust library or required SPARQL worker executable after migration.

The x86-64 T0 build of Habu master 141823c4 runs the native SPARQL suite.
PostgreSQL 18.7 passes all 316 syntax cases and 532 of 539 evaluation cases
with exact RDF terms, including all 24 ordered fixtures. The seven numeric
lexical mismatches are recorded in test/sparql/README.md. Live MCP transport
still needs a host that permits io_uring.

# Habu SPARQL native boundary

This crate supplies the C ABI declared in `habu-sparql.h`. The public Habu
library is `src/lib/sparql.f`; its design is `docs/sparql.md`.

It embeds the standalone SPARQL parser, optimizer and evaluator. It contains
no Oxigraph store, RocksDB dependency, HTTP client, SQL parser or credentials.
A synchronous source callback supplies an authorized RDF dataset.

Build with `cargo build --locked`. Keep `Cargo.lock` with the crate. The
selected upstream revision is pinned, including its development-version
packages; it is not an assertion that an unreleased package is a stable API.
Upstream dependency changes require replaying the existing conformance corpus.

The callback owns its input lifetime and may not unwind through C. The bridge
copies all input spans, guards query reentry, catches Rust panics at mutating
operations, and records the first source error. Ordinary C obligations remain:
handles and nonempty spans must point to live allocations, and one thread uses
a handle at a time. Freeing an active handle from its callback is invalid;
the native boundary leaves it alive and Habu rejects the call.

Returned quads must have an explicit graph field. Empty named graphs are
reported by the named-graph operations, independently of quads. RDF terms
retain lexical spelling, datatype, language and blank-node identity.

This is a materialized-result boundary. It does not yet offer streaming,
interruption of CPU-only evaluation, or a complete SPARQL-to-SQL compiler.
The source can perform selective PostgreSQL reads, but an unbound pattern can
still be large. The caller's storage adapter owns snapshot and access policy.

The Habu wrapper has a focused test alongside it. Native conformance evidence
is delivered separately, with exact suite, dependency and binary identities;
it must not be reported as execution of the Habu wrapper.

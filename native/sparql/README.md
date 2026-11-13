# Habu SPARQL native boundary

Build with Rust/Cargo:

```sh
cargo build --release --locked --manifest-path native/sparql/Cargo.toml
```

Make `native/sparql/target/release/libhabu_sparql.so` (Linux) or
`libhabu_sparql.dylib` (macOS) visible to the dynamic loader. Build on the target
platform.

The ABI has two functions: `hb_sparql_run` and callback-only
`hb_sparql_reply_set`. Its entire protocol and ownership contract are in
`habu-sparql.h`. Habu exposes only `SPARQL:QUERY`.

`src/lib.rs` owns the call, limits and output; `src/source.rs` pulls source
pages; `src/terms.rs` translates RDF terms. There is no retained native handle,
store, network client or background thread.

The parser and RDF dependencies are pinned to Oxigraph commit
`78734be853a4bec714ba09bbfad790e64f3a1150`. The evaluator is the same revision
with local term-preservation and blank-node-scope fixes; see
[vendor/spareval/UPSTREAM.md](vendor/spareval/UPSTREAM.md). `Cargo.lock` fixes
all transitive versions.

Run the focused native regressions with:

```sh
cargo test --locked --manifest-path native/sparql/Cargo.toml --test rdf_terms
```

The [conformance audit](conformance/README.md) records the W3C profile,
comparison rules and measured results. Native conformance and Habu execution
are separate checks.

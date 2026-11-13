# Habu SPARQL native boundary

Build with Rust/Cargo:

```sh
cargo build --release --locked --manifest-path native/sparql/Cargo.toml
```

Make `native/sparql/target/release/libhabu_sparql.so` (Linux) or
`libhabu_sparql.dylib` (macOS) visible to the dynamic loader. Build on the target
platform; the ZIP's tested binary is Linux x86-64 only.

The ABI has two functions: `hb_sparql_run` and callback-only
`hb_sparql_reply_set`. Its entire protocol and ownership contract are in
`habu-sparql.h`. Habu exposes only `SPARQL:QUERY`.

`src/lib.rs` owns the call, limits and output; `src/source.rs` pulls source
pages; `src/terms.rs` translates RDF terms. There is no retained native handle,
store, network client or background thread.

Dependencies are pinned to upstream Oxigraph commit
`78734be853a4bec714ba09bbfad790e64f3a1150`, using only its standalone parser,
evaluator and RDF types. This is a development revision; `Cargo.lock` fixes
all transitive versions. Native conformance and Habu execution are distinct
checks; see the delivery archive for their recorded status.

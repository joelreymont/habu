# Spareval source

From [Oxigraph `78734be853a4bec714ba09bbfad790e64f3a1150`](https://github.com/oxigraph/oxigraph/tree/78734be853a4bec714ba09bbfad790e64f3a1150/lib/spareval).

Only the evaluator crate is copied. Its workspace dependencies are explicit
and keep the same upstream revision. The included MIT and Apache licenses
are unchanged.

Local query fixes:

- MIN, MAX and SAMPLE return the selected RDF term unchanged.
- IF and COALESCE preserve selected terms across mixed computed branches.
- BNODE(string) allocates fresh identities per solution and treats its string
  as an opaque key. A row releases its scope when consumed.

Only `src/eval.rs`, `src/expression.rs` and `src/lib.rs` differ from upstream
Rust sources. The Habu API and source protocol are unchanged.

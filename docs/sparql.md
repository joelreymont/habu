# SPARQL

One public operation, `SPARQL:QUERY`:

```forth
( query query-length base base-length schema schema-length deadline
  context read write -- )
```

`read` has effect `[ context request length -- batch length ]`.
`write` has effect `[ context bytes length -- ]`.
The exact checked effects are in `src/lib/sparql.f`.

Habu parses SPARQL and compiles one parameterized PostgreSQL statement.
PostgreSQL evaluates joins, paths, expressions, aggregates, ordering and
slicing. Models supply SPARQL text. The host supplies the base IRI, trusted
backend schema, absolute monotonic deadline and callbacks.

## Ownership

`QUERY` owns one arena for syntax, SQL construction and result writing.
Callbacks run synchronously on the caller's task. A returned batch remains
valid until the next `read`; a write span expires when `write` returns.
Both callbacks are stable quotations; pass per-call state through `context`.
Callback and library errors propagate as Habu throws. The host discards
partial output after an error and always closes its cursor and transaction.

The deadline is an absolute `TIME:MONO-NS` value, or zero for no deadline.
Parsing, compilation and output check it and yield to task cancellation.
The host enforces the same deadline during database waits and bounds output
bytes. There is no intermediate-read or result-row cap in the library.

## Database callback

The first `read` receives a descriptor:

```json
{"op":"query","sql":"...","params":["..."],"form":"select","vars":["x"]}
```

The host binds every parameter as text and opens a PostgreSQL cursor. Later
reads receive `{"op":"next"}`. Each reply is a JSON array; `[]` ends the
result. Every SQL row contains one JSONB column, `v`:

| Form | Row value | Output |
| --- | --- | --- |
| SELECT | Variable-to-term binding object | SPARQL Results JSON |
| ASK | One boolean | SPARQL Results JSON |
| CONSTRUCT / DESCRIBE | Object with `s`, `p`, `o` RDF terms | N-Triples |

The host owns authorization and one read-only, repeatable-read snapshot
across cursor creation and all fetches. Zen fetches 64 rows at a time and
applies its output-byte budget. Counts and joins execute before rows cross
this boundary.

## RDF source

The trusted schema is checked as a simple SQL identifier and quoted.
Query-controlled strings enter only bound parameters. The backend provides:

- `sparql_match(s,p,o,g)`: RDF triples; SQL NULL means a wildcard for s/p/o
  and the default graph for g.
- `sparql_graphs()`: named graph terms, including empty graphs.
- `sparql_*` term helpers: RDF expression operations. Expression errors are
  SQL NULL; cancellation and resource errors propagate.

Each graph is a set. Terms use SPARQL Results JSON, with language tags in
lowercase and the xsd:string datatype omitted. Other literal lexical forms
and datatype IRIs remain intact. Ordinary solutions retain duplicates.
`FROM` merges selected supplied graphs, renaming their blank nodes apart.
`DESCRIBE` returns a concise bounded description.

Zen supplies its authorized RDF projection over PostgreSQL views through
these functions. Habu owns neither a connection nor a copied dataset.
Updates have no entry point. A normal remote `SERVICE` throws `E-SERVICE`;
`SERVICE SILENT` preserves its input.

## Build and check

Load the library with the current T0 Habu executable. No Rust crate, dynamic
SPARQL library or separate query executable is required.

```sh
bin/hb --load src/lib/sparql-test.f </dev/null
```

The [W3C audit](../test/sparql/README.md) uses the native Habu compiler and
PostgreSQL with the pinned 855-case query profile. Its fixture loading and
comparison policy are preserved from the earlier audit. The old Rust score
is historical; it does not validate this replacement. Native execution and
conformance remain pending until a current T0 runtime runs these checks.

The private [compiler contract](../src/lib/sparql/README.md) records current
implementation limits separately from the public callback API.

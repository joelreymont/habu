# SPARQL

Habu parses a read-only query and compiles one parameterized PostgreSQL
statement. PostgreSQL evaluates joins, paths, expressions, aggregates,
ordering and slicing. The host supplies the base IRI, trusted backend
schema, absolute monotonic deadline and callbacks.

A native database host uses `SPARQL:QUERY-ROWS`:

```forth
( query query-length base base-length schema schema-length deadline
  context execute-plan write -- )
```

The `execute-plan` callback receives `context` and a scoped `^SPARQL:call`.
Read the SQL with `SPARQL:STATEMENT$`, bind its text parameters in
`SPARQL:EACH-PARAMETER` order, and pass each returned SQL column to
`SPARQL:WRITE-ROW` while the database owns the row. Both the statement
and its arguments stay valid for this callback. `SPARQL:GRAPH-RESULT?`
identifies N-Triples output. Each synchronous `write` receives
`context bytes length`.

Hosts with a byte transport can use `SPARQL:QUERY`. It takes a `read`
callback in the same position as `execute-plan`. Its checked effect is
`[ context request length -- batch length ]`. Both interfaces use the
same compiler and result writer.

## Ownership

Each invocation owns an arena for syntax and SQL construction.
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
across cursor creation and all fetches. Zen fetches up to 64 rows,
stopping once a batch reaches 64 KiB. A row stays whole. PostgreSQL
checks its size before transfer, and Zen enforces a 1 MiB output budget. Counts and joins execute before rows cross
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
PostgreSQL with the pinned 855-case query profile. The runner checks row order for indexed result fixtures and
exits with a failing status if any result differs.

The private [compiler contract](../src/lib/sparql/README.md) records current
implementation limits separately from the public callback API.

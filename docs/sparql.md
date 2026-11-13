# SPARQL

One public operation: `SPARQL:QUERY`.

```forth
( request length context read write -- )
```

`read` has effect `[ context request length -- reply length ]`.
`write` has effect `[ context status bytes length -- ]`.
The exact checked effects are in `src/lib/sparql.f`.

The caller supplies SPARQL text, optional typed variable bindings and explicit
limits in one JSON request. The C header is the sole definition of this small
byte protocol: `native/sparql/habu-sparql.h`.

## Ownership

The call owns parsing, evaluation and cleanup. There is no public query object,
prepare/bind/close sequence, form enum, source setter or reply token.
Callbacks use their context for state and run synchronously on the caller's
thread. The read reply is copied immediately. A write span expires when that
callback returns. Habu callback errors are caught before returning through C
and rethrown after the native call returns.

`write` receives result chunks with status zero. A positive status carries a
diagnostic, after which `QUERY` throws. Discard partial output on any failure.
A result limit fails the query; it never turns a partial answer into success.

## Data

The source answers three requests: scan matching quads, list named graphs,
and check a named graph. Scans and graph lists use opaque cursors and pages of
at most 256 items. A page is fetched only when evaluation needs it.

The source owns authorization, one consistent snapshot and RDF-set uniqueness.
It distinguishes the default graph from named graphs, including empty ones.
The bridge checks term types and pattern matches. Terms use SPARQL Results
JSON binding objects, preserving literal spelling, datatype and language.

For Zen, PostgreSQL remains the store. Its source maps existing authorized
views to RDF; the library has no database handle, SQL, storage engine or
network client. `FROM` selects supplied graphs. Updates have no entry point,
and no remote `SERVICE` handler is installed.

## Evaluation

Spargebra parses standard SPARQL; Spareval evaluates it. These standalone
crates share a pinned upstream revision and a locked dependency graph. No
Oxigraph store or RocksDB is used.

SELECT and ASK stream SPARQL Results JSON. CONSTRUCT and DESCRIBE stream
N-Triples. Source pages, result rows, result bytes and callback deadlines are
bounded by the caller's request. Joins and sorting can still buffer data inside
the evaluator; this is not a compiler that pushes the whole query into SQL.

Deadline checks at callbacks cannot interrupt arbitrary CPU work inside the
parser or evaluator. Zen runs the call in a short-lived process using its
existing process timeout. That also keeps native evaluation off the server's
cooperative scheduler. A timeout kills the worker and discards its output.

## Build and check

Build `native/sparql` with Cargo and make its dynamic library available to Habu.
`native/sparql/README.md` gives the command. With a trusted installed `hb`, load
`src/lib/sparql-test.f` for the focused callback/exception check.

The delivery archive contains the W3C conformance harness and measured results.
Those execute the native ABI. They do not establish that the Habu wrapper or
Zen worker has compiled; both require a trusted `hb` run.

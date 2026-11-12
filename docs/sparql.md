# SPARQL for Habu

## Decision

Expose standard SPARQL Query through a small Habu library. Keep the dataset
in its owner's storage. For Zen, that owner is PostgreSQL.

The library parses and evaluates queries. A source supplies authorized RDF
quads and named graph identities from one snapshot. Zen owns its ontology,
fact projection, permissions, evidence and write operations.

The implementation uses the standalone `spargebra` parser and `spareval`
evaluator through a small C ABI. It does not use the Oxigraph database or
RocksDB. The exact upstream commit and every transitive version are locked in
`native/sparql/Cargo.toml` and `Cargo.lock`.

This boundary avoids writing another incomplete query grammar and expression
evaluator. Habu retains its checked public interface, ownership and provider
callbacks. The foreign code owns standards machinery, as libpq owns the
PostgreSQL protocol in Habu's existing database library.

## Vocabulary

Four nouns are enough:

| Noun | Meaning |
| --- | --- |
| `query` | One parsed query, its bindings, result and diagnostic |
| `source` | A checked provider and its caller-owned context |
| `form` | SELECT, ASK, CONSTRUCT or DESCRIBE |
| `reply` | A borrowed destination for one provider response |

The package is `SPARQL`. The model sends SPARQL text; it never constructs a
Habu query DSL or sees the native algebra.

| Word | Contract |
| --- | --- |
| `PARSE` | Read query text and a base IRI into a query |
| `FORM` | Return its query form |
| `BIND` | Supply one variable's RDF term without interpolating query text |
| `QUERY` | Evaluate against a source |
| `RESULT$` | Borrow the serialized result |
| `ERROR$` | Borrow the diagnostic |
| `DATASET$` | Inspect the query's FROM/FROM NAMED selection |
| `CLOSE` | Release the query; closing an already closed record is harmless |
| `WITH-QUERY` | Run a body and close its query on return or throw |
| `SOURCE!` | Install the checked source provider |
| `REPLY!` | Copy a provider response into its borrowed reply |

The last two words are for storage adapters. No public words build joins,
filters, paths, expressions or SQL. Those operations already have SPARQL
syntax and semantics. Full checked effects and ownership rules live with the
implementation in `src/lib/sparql.f`.

## Query lifecycle

Declare a zero-filled `SPARQL:query` and `SPARQL:source`. Install a provider,
parse the query, bind any initial terms, execute, consume the result, and close.
Reparsing replaces the previous plan and clears its bindings and result. A
syntax failure leaves no old plan available for accidental execution.

Records belong to their caller. One task uses a record at a time. A live
record is not copied. `WITH-QUERY` supplies exception-safe cleanup; explicit
`CLOSE` is available where a longer lifetime is useful.

Source callbacks run synchronously on the calling thread. Their request spans
and reply token expire on callback return. Result and diagnostic spans expire
when the query changes or closes. A consumer that retains bytes copies them.
There are no background threads or hidden store handles.

Reentry into an active query or source is rejected. A provider throw is caught
inside the Habu callback and rethrown after the native call returns. The source
record carries that error; it is never stored in a process-wide callback error
cell. Native provider failures are latched, so an evaluator branch cannot turn
a failed read into an empty successful answer.

## Source contract

The provider has three operations: scan matching quads, enumerate named graphs,
and test whether a named graph exists. The complete byte protocol is defined
once in `native/sparql/habu-sparql.h`.

A quad has subject, predicate, object and graph. The graph is explicitly null
for the default graph. A missing graph field is an error. Named graph identity
is kept separately from its triples so an empty named graph remains visible.

Graph selection has three distinct meanings:

| Selection | Matching data |
| --- | --- |
| Default | Only the default graph |
| Named | One specified named graph |
| Any | Every named graph, excluding the default graph |

The provider returns only quads matching the requested pattern. The bridge
checks this and rejects invalid RDF terms, literal subjects or predicates, and
out-of-pattern results. Quads have RDF-set semantics. Solution rows have SPARQL
bag semantics and retain their multiplicity.

Terms use SPARQL Results JSON binding objects: IRI, blank node, or literal with
its lexical value and optional datatype or language tag. An unbound variable
has no binding. It is not an empty string, numeric zero, or an unknown fact.
Literal identity must survive storage and lookup; the lexical integer `01`
must not silently become `1` before a query examines it.

The provider controls access. Passing an inaccessible graph name in a query
does not grant access to it. FROM selects graphs already supplied by the
provider; the bridge never dereferences a dataset IRI.

## PostgreSQL and Zen

The intended MCP call is `query(base, sparql, bindings)`. Its implementation is:

1. Resolve `base` to the caller's authorized Zen context.
2. Open a read-only PostgreSQL snapshot for the whole evaluation.
3. Install a source bound to that context and transaction.
4. Parse and bind the SPARQL query; execute it through the source.
5. Resolve returned fact identities through Zen's existing evidence tools.

The PostgreSQL adapter translates bound RDF terms into parameterized reads of
the relevant authorized views. Relation names and SQL structure come from the
adapter. Model text and RDF lexical values are never interpolated into SQL.
The source must retain the same snapshot across every callback.

The conformance provider executes such bound scans against indexed PostgreSQL
fixture tables. These tables are a test dataset, not a proposal to replace
Zen's schema with a new triple database. A production Zen provider should map
its existing authorized views and preserve its fact/evidence identities.

The evaluator performs joins, expressions, aggregation and paths. Pattern
lookups can be selective in PostgreSQL without exporting the whole graph at
the start of every query. This is not whole-query SQL pushdown: a broad pattern
can still fetch many rows, including when a later LIMIT returns few results.

Zen's graph design remains separate. Ordinary active, unconditional exact
assertions can be exposed as direct triples where their meaning is preserved.
Conditions, comparison operators, alternatives, units, conflicting assertions
and evidence require explicit fact resources. A ceiling expressed by `<=` must
not be projected as an equality. This library does not invent that ontology.

## Query profile

The target is SPARQL 1.1 Query under simple entailment, including SELECT, ASK,
CONSTRUCT, DESCRIBE, expressions, aggregation, subqueries and property paths.
The engine supplies expression semantics; there is no private function
allowlist. DESCRIBE follows the selected evaluator's description algorithm,
which the standard permits implementations to choose.

Update operations have no public entry point. There is no HTTP endpoint, graph
store protocol, RDFS/OWL reasoner or remote SERVICE handler in this library.
Valid SERVICE syntax can parse, but no remote request is made; the evaluator's
normal error/SILENT behavior applies. Zen can impose a stricter execution
policy at its boundary, separately from syntactic validity.

Dataset selection is logical and local. SQL, connection credentials and access
policy are absent from the native parser/evaluator API. The provider owns
them. Bindings are typed RDF values, not query fragments.

## Build and verification

Build the native boundary from the Habu checkout:

```sh
cd native/sparql
cargo build --locked
```

The output is `libhabu_sparql.so` on Linux or `libhabu_sparql.dylib` on macOS.
Make that directory available to the platform dynamic loader, then use a
trusted Habu compiler from the checkout root:

```sh
bin/hb --load src/lib/sparql-test.f </dev/null
```

The focused Habu test exercises standard ABS, an actual provider callback,
syntax failure, provider exception propagation and cleanup. It does not
reimplement the W3C suite as hundreds of local unit tests.

The external conformance audit traverses the pinned W3C manifests and invokes
this native ABI for every selected case. It compares exact RDF terms, duplicate
rows and globally consistent blank-node identity. It uses the suite's declared
ordering and REDUCED rules. The same queries are evaluated with memory and
PostgreSQL sources. All mismatches remain in the primary score, including
numeric lexical differences; their classification is explanatory only.

The audit belongs in the delivered evidence bundle. It is an external Python
research harness, not a new Python tool or runtime in the Habu repository.
The Habu source and its focused test require a real `hb` run before their
checked effects and callback behavior are considered verified.

## Remaining implementation work

The implementation is a branch candidate, not a production Zen cutover.

- Load and run the Habu boundary on a supported host with a trusted `hb`.
- Resolve the recorded upstream BNODE identity mismatch and review the exact
  numeric lexical mismatches against the selected conformance profile.
- Implement Zen's authorized PostgreSQL source and fact projection.
- Give production requests bounded result delivery and cancellation. The
  current boundary materializes each pattern reply and the final result.

Keep the public vocabulary stable while doing this work. Do not compensate
for a failing ordinary expression by adding a new private query dialect.

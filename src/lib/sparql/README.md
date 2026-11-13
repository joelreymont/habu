# Native query compiler

Private implementation of `SPARQL:QUERY`. The caller owns one arena. Habu
parses the query and emits one parameterized PostgreSQL statement; PostgreSQL
evaluates joins, paths, aggregates, ordering and slicing.

## Internal contract

`QUERY(query, base, schema, deadline, context, read, write)` owns the arena.
`read(context, request)` returns an array of row values; after the plan
descriptor, each request is `{"op":"next"}` and an empty array ends the
stream. `write(context, bytes)` receives standard SPARQL Results JSON or
N-Triples chunks. Callback failures propagate after cleanup. The host accepts
the output only after the whole query and transaction succeed.

`PARSE-UNTIL(query, base, deadline, arena)` produces syntax. A deadline is an
absolute `TIME:MONO-NS` reading, or zero. `PARSE` supplies zero for parser
tests. Parsing and SQL construction check the deadline and yield to task
cancellation; recursive syntax/evaluation lowering is bounded at 256 levels.

`COMPILE(syntax, schema)` produces a plan. The schema is a trusted host
argument, checked as a simple SQL identifier and quoted. Query-controlled
text appears only in text parameters. `PLAN-REQUEST$` returns:

```json
{"op":"query","sql":"...","params":["..."],"form":"select","vars":["x"]}
```

The statement returns one JSONB column, `v`: a binding object for SELECT,
one boolean for ASK, or an `{s,p,o}` object for CONSTRUCT/DESCRIBE. SELECT
preserves explicit ordering through projection, distinctness and slicing.
DESCRIBE uses the concise bounded description of its selected resources.

The fixed backend functions live in the trusted schema:

- `sparql_match(s,p,o,g)` returns matching RDF triples. SQL NULL means a
  wildcard for s/p/o and the default graph for g.
- `sparql_graphs()` returns named graph terms in `g`; an optional NULL row
  denotes the default graph.
- `sparql_*` term helpers implement RDF operators and functions. An
  expression error is SQL NULL; cancellation and resource errors propagate.

Each graph is a set. Source and helper terms use SPARQL Results JSON with
`xml:lang` in lowercase and no datatype field for xsd:string. Other literal
lexical forms and datatype IRIs remain intact. This permits direct JSONB
identity comparisons in joins. UNION and ordinary joins retain duplicate
solutions; DISTINCT and graph construction remove duplicates deliberately.

The host owns authorization, the read-only snapshot, a PostgreSQL cursor,
the deadline and an output-byte budget. No intermediate-read or result-row
cap is imposed by this compiler. SERVICE has no remote executor: a normal
SERVICE throws `E-SERVICE`, and SERVICE SILENT preserves its input.

## Verification boundary

This is source under development, not a new conformance result. The former
Rust engine's recorded 839/855 score does not validate this implementation.
Native load, PostgreSQL execution and the pinned W3C profile must be rerun
with a current T0 Habu executable. The draft still lacks codepoint escapes
outside strings/IRIs and complete RFC 3987 IRI validation. Backend XPath/IEEE
edge cases also require execution evidence. These limits are not a new
SPARQL profile.

`tools/sparql-compile.f` accepts a JSON array of `[query, base-IRI]` pairs and
prints one descriptor or numeric error per line, in one Habu process:

```sh
bin/hb --load tools/sparql-compile.f -- batch.json test_backend </dev/null
```

The [external W3C harness](../../../test/sparql/README.md) uses those statements
without evaluating SPARQL itself. Its historical fixture/comparison functions
remain unchanged. The obsolete Rust crate, ABI and evaluator are removed;
there is no separate query executable or library to build.

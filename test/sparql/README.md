# SPARQL conformance

Habu parses and compiles queries. PostgreSQL evaluates the emitted SQL.
RDFLib loads fixtures and compares results; it never evaluates SPARQL.
The public Habu result writer and HTTP MCP host require separate checks.

## Profile and comparison

The pinned W3C RDF tests revision is
`7b5afb91857a9c282c03d2e45d3f76485797c901`. The selected SPARQL 1.0/1.1 query
profile contains 316 syntax cases and 539 evaluation cases: 855 total. It
excludes updates, entailment, remote federation execution, protocol and
SPARQL 1.2. Valid federation syntax is still accepted independently of the
execution policy.

`fixtures.py` preserves the previous fixture/comparison functions verbatim.
Comparison checks exact RDF 1.1 terms, duplicate rows and one blank-node
bijection across the whole result. Numeric lexical spelling is preserved.
Ordering follows explicit `rs:index` annotations; this profile has none, so
the score alone cannot prove every ORDER BY sequence. REDUCED uses the
fixture's lax cardinality rule. No numeric mismatch is reclassified as a pass.

`dataset-descriptions.json` contains input graph selections from the
historical parser, checked against each query's hash. It contains no expected
answer. The current compiler must perform the actual FROM selection and merge.
The downloaded suite retains its own licenses.

## Replay

Use a current T0 Habu executable and a disposable PostgreSQL 18 connection.
From the repository root:

```sh
python3 -m venv /tmp/habu-sparql-audit-venv
. /tmp/habu-sparql-audit-venv/bin/activate
python -m pip install -r test/sparql/requirements.txt
git clone https://github.com/w3c/rdf-tests \
  test/sparql/rdf-tests-7b5afb91857a9c282c03d2e45d3f76485797c901
git -C test/sparql/rdf-tests-7b5afb91857a9c282c03d2e45d3f76485797c901 \
  checkout 7b5afb91857a9c282c03d2e45d3f76485797c901
python test/sparql/export_readonly.py
```

Set `ZEN_VALIDATION_CONNINFO` to the disposable PostgreSQL connection and run:

```sh
python test/sparql/run_habu.py \
  --hb bin/hb \
  --projection ../zen/sql/sparql.sql \
  --terms ../zen/sql/sparql-terms.sql \
  --output .out/sparql-conformance/results.json
```

The adapter loads `tools/sparql-compile.f` once for all queries. It retains
every emitted plan, compiler diagnostic and source hash with the report.
The test backend uses temporary fixture tables and the production SQL term
helpers. Each query runs in one read-only snapshot with a 10-second statement
deadline and 64-row cursor batches. There is no intermediate-read or result-row
cap. `--syntax-only` runs the 316 syntax cases without a database.

## Result boundary

No result is recorded for this Habu/PostgreSQL replacement yet. A compiler
load failure is an audit failure, not a skipped test or a conformance score.
The adapter covers Habu parsing/SQL construction and PostgreSQL evaluation;
it does not execute the public Habu result writer or MCP transport.

The former Rust implementation recorded 839/855. Its source, ABI probes and
result artifacts remain in Git history at `50baa2a`; they were removed from
the current tree with the obsolete crate and build route. That historical
score must not be reported as a result for this engine.

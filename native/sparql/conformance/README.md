# Native SPARQL conformance

This audit calls Habu's native C ABI. RDFLib loads fixtures and compares
results; it never evaluates SPARQL. The Habu wrapper and HTTP MCP host are
outside this audit.

## Recorded result

The pinned 855-case W3C SPARQL 1.0/1.1 query profile passes **839/855** with
the memory source, up from **835/855**. The three MIN failures and BNODE test
now pass. All other outcomes are unchanged; there are no harness errors.

[results.json](results.json) records every test status, complete failure
records, source hashes and the exact native binary hash. Successful answer
payloads are omitted from that checked-in record; replay emits full records.

The [native regressions](../tests/rdf_terms.rs) pass three tests containing ten
queries through the C ABI. The additional [blank-node probes](bnode.py) pass
[10/10](bnode.json), and [execution controls](controls.py) pass
[9/9](controls.json). Their query and callback behavior is unchanged from the
executed probes; `bnode.py` only drops the original workspace import path.

PostgreSQL was not rerun for this revision: the environment refused Unix
socket creation in both the workspace and `/tmp`. These are native engine
fixes; the PostgreSQL source and Habu ABI did not change. Habu-language and
HTTP MCP execution remain unverified here.

## Comparison

Comparison preserves RDF terms, duplicate rows and a blank-node bijection
across the whole result. Numeric spelling is not normalized. Ordering follows
explicit `rs:index` annotations; this profile has none, so its score alone
does not prove all ORDER BY sequences.

Sixteen strict failures remain: fifteen computed numeric representation
differences and one TSV fixture whose `1.0e6` differs from input `1.0E6`.
The [W3C comparison instructions](https://www.w3.org/2009/sparql/docs/tests/README.html)
and [numeric representation discussion](https://github.com/w3c/rdf-tests/issues/58)
explain why value comparison needs a separate policy. Normalizing every
number would hide the term-preservation defect just fixed. No failure is
reclassified as a pass.

## Replay

From the repository root:

```sh
cargo build --locked --manifest-path native/sparql/Cargo.toml
cargo test --locked --manifest-path native/sparql/Cargo.toml --test rdf_terms
python3 -m venv /tmp/habu-sparql-audit-venv
. /tmp/habu-sparql-audit-venv/bin/activate
python -m pip install -r native/sparql/conformance/requirements.txt
git clone https://github.com/w3c/rdf-tests \
  native/sparql/conformance/rdf-tests-7b5afb91857a9c282c03d2e45d3f76485797c901
git -C native/sparql/conformance/rdf-tests-7b5afb91857a9c282c03d2e45d3f76485797c901 \
  checkout 7b5afb91857a9c282c03d2e45d3f76485797c901
python native/sparql/conformance/export_readonly.py
python native/sparql/conformance/run_native.py --provider memory \
  --library native/sparql/target/debug/libhabu_sparql.so \
  --output /tmp/habu-sparql-memory-results.json
```

Use the locally built `.dylib` on macOS. For PostgreSQL, set
`ZEN_VALIDATION_CONNINFO` to a disposable PostgreSQL 18 connection and select
`--provider postgres`; the provider uses temporary fixture tables. Both
`controls.py` and `bnode.py` accept `--library PATH --output PATH`.

`dataset-descriptions.json` contains input graph selections captured from the
unchanged parser revision, checked against each query's hash. The profile
excludes updates, entailment, remote federation, protocol and SPARQL 1.2.
The downloaded W3C suite retains its own licenses. The audit is explicit and
is not added to normal Habu builds.

# SPARQL implementation status

The Habu API and native bridge are implemented on the `sparql` branch.
The native boundary was executed. The Habu wrapper was statically reviewed,
but has not been loaded or typechecked by `hb` in this environment.

## Pinned subjects

- Habu starting revision: `a8ead9c1f480e8d10c1a191e45789eeb48302d14`.
- W3C `rdf-tests`: `7b5afb91857a9c282c03d2e45d3f76485797c901`.
- Parser/evaluator source: Oxigraph's standalone libraries at
  `78734be853a4bec714ba09bbfad790e64f3a1150`.
- Native artifact SHA256:
  `2aa528cac54bab5cda3bd5cd263330efb0b1298aa1052116a475af80a3eb203f`.

The selected upstream packages are development versions: `spargebra 0.5.0-dev`,
`spareval 0.3.0-dev`, and `oxrdf 0.4.0-dev`. The Git revision and Cargo lockfile
make the selection reproducible. No upstream semantic patch is applied.

## Measured results

| Executed boundary | Pass | Fail | Harness errors |
| --- | ---: | ---: | ---: |
| Query syntax, either provider | 316 / 316 | 0 | 0 |
| Query evaluation, memory provider | 519 / 539 | 20 | 0 |
| Query evaluation, PostgreSQL 18 provider | 519 / 539 | 20 | 0 |
| Combined query profile, each provider | **835 / 855** | **20** | **0** |
| Focused native ABI checks | 8 / 8 | 0 | 0 |

The providers agree on every case's pass/fail status. PostgreSQL runs parameterized
pattern reads in a read-only, repeatable-read transaction; it is an actual
PostgreSQL server, not a SQL-string simulation.

The 20 remaining mismatches comprise 19 numeric lexical differences and one
`BNODE(string)` identity mismatch across solution rows. Numeric value-equivalent
results remain failures in the strict score. The BNODE behavior can merge
identities that should be fresh across solutions and requires an upstream fix.

The released dependency combination (`spargebra 0.4.8`, `spareval 0.2.9`) scored
815/855 under the same harness. The selected commit fixes 21 semantic-result
mismatches and all syntax mismatches. It also changes some numeric lexical
outputs. This evidence is the reason for choosing the exact development revision.

The ABI audit found and fixed a bridge defect: a missing graph field was treated
as default-graph membership. The bridge now requires an explicit graph field.
It also verifies source-failure propagation, old-query invalidation, empty named
graphs, reentry refusal, RDF term identity and binding values that resemble
query injection.

## Suite scope

The canonical W3C roots contain 1,406 entries at the pinned revision, across
SPARQL 1.0, 1.1 and 1.2. The read-only 1.0/1.1 query profile contains 855.
It follows the authoritative manifest lists rather than counting every file.
No exact 1,900-case suite was identified.

Other 1.0/1.1 categories are recorded outside this profile: 149 update cases,
70 entailment cases, seven federation evaluations, four CSV serialization
cases, 34 query/update HTTP protocol cases, 13 graph-store HTTP protocol cases,
and three service-description cases. The 271 SPARQL 1.2 cases are outside this
target. These are not counted as passes.

The comparator preserves exact RDF literal identity, bag multiplicities and a
global blank-node bijection. It uses `rs:index` ordering and `mf:LaxCardinality`
only where the suite specifies them. No expected files are edited, no expected
answer is fed to the evaluator, and no reference engine evaluates these queries.
RDFLib only loads fixtures/results and compares RDF graphs.

## Explicitly unverified

- The Habu wrapper and focused Habu test have not executed. This Linux x86-64
  environment has no trusted `hb`; Habu's bootstrap instructions require an
  existing compiler and disable x86-64 builds pending the linker transition.
- The production Zen provider, authorized fact projection and live MCP route
  are not implemented by this library branch.
- Whole-query SQL pushdown, streaming and bounded CPU-only cancellation are
  not implemented. Each matching-pattern reply and final result is materialized.
- Passing this finite profile would not by itself establish complete SPARQL
  conformance; the recorded profile currently has explicit failures.

The complete external audit, every result, suite archive, native artifact,
reproduction instructions and session history accompany the delivered ZIP.

## Primary references

- [W3C SPARQL suites](https://w3c.github.io/rdf-tests/sparql/)
- [W3C comparison and manifest rules](https://www.w3.org/2009/sparql/docs/tests/README.html)
- [Pinned suite](https://github.com/w3c/rdf-tests/tree/7b5afb91857a9c282c03d2e45d3f76485797c901)
- [Pinned evaluator](https://github.com/oxigraph/oxigraph/tree/78734be853a4bec714ba09bbfad790e64f3a1150/lib/spareval)

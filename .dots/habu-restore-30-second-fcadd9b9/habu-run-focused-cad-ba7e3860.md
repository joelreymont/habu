---
title: Run focused CAD replay case
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T02:12:07.822265+02:00\""
---

Why: maki/cad-replay-test.f launches all of maki/cad-test.f to prove one
fresh-process persistence invariant. This duplicates unrelated work and makes
the replay test inseparable from the core slice. The focused case and wrapper
also need one private CAD store so concurrent Maki slices cannot race through
the default tmp/cad-store.

Owner and interface: add package-owned standalone maki/cad-replay-case.f
containing only the existing fresh-process persistence scenario. It defines the
same FFN model, writes selection 7 through the real durable CAD store, renders
through production TILE, launches the child through
ENGINE-CANDIDATE:PATH$, and proves the child reports the persisted selection
rather than defaults. It exposes no reusable production or test-only API.
Register the case exactly once in the canonical full Maki inventory and the
existing core slice. maki/cad-replay-test.f runs only that case under its
existing content-distinct executable clone. The wrapper creates one private
root and passes exact HABU_UNDER_TEST, HABU_CAD_STORE, and TMPDIR values to
the case; the case preserves those values for its nested child. The cloned
parent and nested child therefore use the same executable identity and store
root. Missing or empty required roots fail before spawning. Every private
store, driver, and clone root is removed on success and failure without
replacing the primary throw. Remove only the moved fresh-process block and its
now-dead support from maki/cad-test.f; every retained same-process assertion
and every line outside that block remains byte-identical. Keep cad-test,
cad-replay-case, and cad-replay-test in the core slice in this leaf; Maki slice
rebalancing is a dependent change.

Checkpoint: before substantial edits, reproduce the real clone failure by changing only the case engine resolver to bin/hb, and record the baseline per-suite time. Run a representative packaged case definition through the package and typed-local gates; stop if the case requires a new CAD, store, engine, process, or test-framework interface.

Acceptance: the registered current-engine case and marked-clone wrapper both
run the real persistent replay path and exit cleanly. Hardcoding bin/hb makes
the clone path fail the stored-selection and no-default assertions. Omitting
the durable row, changing its model identity, changing either propagated engine
or store root, or allowing the case to use the default tmp/cad-store also fails
through production TILE. A structural check proves cad-replay-test.f no longer
loads cad-test.f, the new case appears exactly once in the full inventory and
core slice, and every retained same-process line in cad-test.f is byte-identical.
Run the case and wrapper concurrently with distinct private stores and prove
both pass without touching tmp/cad-store. No copied CAD logic, parser,
validator, public test helper, timing-threshold change, workload reduction,
suite redistribution, retry, cache bypass, or compatibility alias is added.
Files: maki/cad-replay-case.f, maki/cad-replay-test.f, maki/cad-test.f,
maki/test.f, maki/test-core.f, FILEMAP.md, and the existing suite-coverage
inventory only if registration requires it. Smallest checks:
bin/hb --load maki/cad-replay-case.f,
bin/hb --load maki/cad-replay-test.f, and
bin/hb --load maki/cad-test.f. Verify the hostile engine/store mutations,
exact package and typed-local diff lints, all three focused tests, suite
coverage, standalone Maki, candidate validation, and the exact native gate.
Depends: no type-system work. Ownership: only the focused fresh-process CAD
replay path, its suite registration, private environment propagation, and
removal of its duplicated whole-suite execution.

Claim: agent=cad_replay_impl workspace=.jj-ws/habu-run-focused-cad-ba7e3860.

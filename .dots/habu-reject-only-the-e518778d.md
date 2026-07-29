---
title: Reject only the expected field projection miss
status: active
priority: 1
issue-type: task
created-at: "2026-07-21T22:04:23.255435+02:00"
---

src/core/type-family.f runs field projection under catch and drops every thrown code even though only the documented E-PF-ID miss is expected. Any future capacity, corruption, protection, or checker error is silently converted into an ordinary no-field result. Refactor the projection boundary to return an explicit typed found/not-found result for the expected lookup miss, or catch and classify exactly E-PF-ID while rethrowing every other code unchanged. Preserve successful projection, legitimate absence, package visibility, and diagnostics. Add injected throws for every sibling error class and one unknown code, proving only the exact miss becomes absence; corruption and capacity errors propagate with their original identity. Add positive and absent field cases across products, enums, custom layouts, and nested projections. Do not use a range or heuristic over error values. Files: type-family projection owner and focused checker/type-family tests. Verify exact prefix load, type-family/declaration/rollback suites, native fixpoint, bootstrap parity, trust/package/host/filemap/dot lints, Maki, PTX standard library, and full native gate.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `projection_miss` and workspace `.jj-ws/habu-reject-only-the-e518778d` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `src/core/type-family.f:2530` still reads `[: TFAM-FIELD-PROJ-DO ;] catch drop`, so every thrown code is still swallowed with no classification or rethrow. The dot stays active and is free to claim.

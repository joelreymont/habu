---
title: Reject only the expected field projection miss
status: active
priority: 1
issue-type: task
created-at: "2026-07-21T22:04:23.255435+02:00"
---

`src/core/type-family.f` runs field projection under `catch` and drops every
thrown code even though only the documented `E-PF-ID` miss is expected. A
capacity, corruption, protection, or checker error is therefore converted into
ordinary absence.

Refactor the projection boundary to return an explicit typed found/not-found
result for the expected lookup miss, or classify exactly `E-PF-ID` and rethrow
every other code unchanged. Preserve successful projection, legitimate
absence, package visibility, and diagnostics. Inject every sibling error class
and one unknown code; only the exact miss becomes absence, while corruption and
capacity errors retain their identity. Add positive and absent cases across
products, enums, custom layouts, and nested projections. Do not use a value
range or heuristic.

Files: the type-family projection owner and focused checker/type-family tests.
Verify the exact prefix load, type-family/declaration/rollback suites, native
fixpoint, bootstrap parity, package and host gates, Maki, PTX standard library,
and the full native gate.

Claim: agent=projection_miss workspace=.jj-ws/habu-reject-only-the-e518778d

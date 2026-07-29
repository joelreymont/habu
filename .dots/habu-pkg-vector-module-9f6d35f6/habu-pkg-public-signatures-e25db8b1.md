---
title: Package public-signatures core module
status: active
priority: 2
issue-type: task
created-at: "2026-07-27T08:47:43.798709+02:00"
blocks:
  - habu-pkg-intern-lint-e735c0f6
  - habu-tools-reflect-all-80b1aa58
---

`tools/public-signatures-core.f` defines 224 global words and exposes mutable
scanner state solely so tests and gate code can drive internals. This blocks
package-owned edits and lets callers mutate tool state. Move the complete core
into package `PUBSIG`; its only public word is `MAIN ( -- )`.
`tools/public-signatures.f` owns package `PUBSIG-CLI` and calls
`PUBSIG:MAIN`. Package the suite as `PUBSIG-TEST`. Gate and test consumers
must invoke the real command through `RUN-ARGV-CAPTURE`; they may not access a
buffer setter, trust-mode cell, JSON phase, scanner phase, usage word, or
constructor-signature builder.

Delete the six in-process output-capture words and nine backing cells after a
resolved-owner census proves they have no callers. Rename every surviving
`PS-*` definition to a short package-local tail and keep it private. Preserve
the checked 224-definition rename map as the source-to-target audit, excluding
only definitions proven dead and deleted. Do not add forwarding aliases,
exports for tests, raw mutable state, `SUMTYPE`, or `PRODUCT`.

Use one package-owned child preload fixture containing a unified compact
`ENUM` and `STRUCTURE`, then run the real CLI over it. This leaf does not
add payload-`ENUM` or `STRUCTURE` constructor rows: existing dot
`habu-tools-reflect-all-80b1aa58` owns those semantics and the 23-parameter
generic proof. Preserve the exact `examples/llm/good.f` outputs: JSON is 1,982
bytes with SHA-256
`cbbae87e9df05319d457be3fbda5f3d30f25ec7e7ff8411c0a9de2a2ec6e0424`;
trust output is 435 bytes with SHA-256
`739b2592ad080852cb87ca91cec7f30a2648102effb24c9b61ede0b9aafdafc7`;
both stderr streams are empty.

Exact write set: `tools/public-signatures-core.f`,
`tools/public-signatures.f`, `tools/public-signatures-test.f`, and
`test/gate-diagnostics-lib.f`. Acceptance must prove through the production
resolver that PUBSIG's public wordlist is exactly `MAIN`; every retired
`PS-*` and test `PST-*` global is unresolved; representative private tails
are unresolved outside PUBSIG; the CLI fixture and both pinned good-file modes
pass; the standalone, diagnostics, standard-library-manifest, host, file-map,
typed-local, and package gates pass. The long-term solution is package
ownership with one command boundary, not a namespace veneer.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `claude-pubsig-r2` and workspace `.jj-ws/habu-pubsig-r2` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `rg PUBSIG tools test` returns nothing and `tools/public-signatures-core.f` still defines its `PS-` words at global scope with no `package` line. The dot stays active and is free to claim.

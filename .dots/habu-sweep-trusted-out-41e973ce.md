---
title: "Sweep TRUSTED: out of the core and compiler"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T15:49:57.931374+03:00"
---

Retire remaining unchecked bodies under src/core, src/compiler, src/habu and
src/os. Ordinary Habu becomes checked code; genuinely unmodeled engine,
syscall and FFI operations stay small explicit primitive boundaries owned by
their package. Do not substitute an axiom for an ordinary checked word.

The old B7/B8 row-recording and authority mechanism is implemented, including
the declared-row window path and the declaration shim removal. Its historical
source counts and schedule are not current prerequisites. The stricter
package boundary/final flag work remains with e08e345f and 42b30edd.

When resumed after builds and test-suite work, inspect the current definitions
and their real load paths. Known remaining candidates include generated
declaration raw-memory/runner bodies, structure-make's row emitter and
verify-source's checker dispatch. Probe before replacing each one: checked
bodies use their actual bound effects; representation changes use explicit
casts; source-window/owner access must work at the responsible boundary.
Keep multi-call or raw-memory bodies separate from simple forwarding shims.

Use one coherent change per owner/file family, preserving existing positive
and rejection behavior. Before adding tests, inspect the current downstream
and source-window E2Es. Acceptance is no TRUSTED: definitions in the affected
src surface, a matching engine, the required full native suite and convergence
for shared checker/compiler/capture changes. Recovery checks apply when a
seed, mirror or recovery dependency changes. Do not repeat completed row
recording, resurrect obsolete temporary probes or weaken required boundaries.

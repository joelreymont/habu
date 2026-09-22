---
title: Eliminate the stripped code the surface census cannot reach
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T18:25:08.476546+03:00"
---

Problem: the engine-size census (b7293ede) on the a7478993 engine leaves 3127 stripped spans (66,980 code bytes) unreachable from the dictionary-surface roots and 3725 (166,684 bytes) from the engine-entry roots. The surface number is a floor: no name resolves to a stripped span and every recorded address is a root, so those 67 KB are dead in every engine and in every stripped application that inherits the blob. Largest by the sidecar (tools/engine-size.f on /tmp/hazel-fEE2 with its .names): ROW-WRITE 864, NAME-SPAN, 500, BIND-SOURCE-CALLS 468, IMK-Q+ 388, IMPORT-CHECK 372, IMK-PKG-PUBLICS 364, PROVIDE-TARGET 364. Acceptance: say why the surface cannot reach them - dead private words, or a reaching mechanism the census does not model (name it and teach the census before removing anything) - then either drop the dead spans at build time (the closure strip or native-build, whichever owns the blob) with three generations and test/run.f proving the engine, or record in docs/compiler-measurements.md why each top span must stay. Files: tools/image-size-lib.f, src/habu/aot-closure.f or tools/native-build-core.f, docs/compiler-measurements.md. Verify: engine-size on the new engine reports the surface unreachable-span floor near zero; test/run.f; generation cmp. Depends: none (habu-measure-reachability-across-7032b341 landed). Ownership: engine size. Claim: unassigned.

Audit 2026-09-22 on the current private release engine (`/tmp/hazel-release/hb`):
the surface census reports 3,120 unreachable anonymous spans carrying 67,088 code
bytes. The engine-entry root set reports 3,718 spans and 166,792 code bytes; that
second number is a lower bound for a future sealed engine, not a deletion list.
The largest surface-unreachable spans are still `ROW-WRITE` (864 bytes),
`NAME-SPAN,` (500), `BIND-SOURCE-CALLS` (464), `IMK-Q+` (384), `IMPORT-CHECK`
(368), `IMK-PKG-PUBLICS` (360), and `PROVIDE-TARGET` (360). The first two are
only in the primitive-table build source; the next two are capture/compiler
support; the remaining rows need a runtime-root check before removal. The
reachability walk is conservative about gaps and relocation rows, but the
capture-side compactor still has to preserve direct fall-through and every
declared code-cell root before it can drop a span.

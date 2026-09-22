---
title: Eliminate the stripped code the surface census cannot reach
status: active
priority: 3
issue-type: task
created-at: "2026-09-21T18:25:08.476546+03:00"
---

Problem: the engine-size census (b7293ede) on the a7478993 engine leaves 3127 stripped spans (66,980 code bytes) unreachable from the dictionary-surface roots and 3725 (166,684 bytes) from the engine-entry roots. The surface number is a floor: no name resolves to a stripped span and every recorded address is a root, so those 67 KB are dead in every engine and in every stripped application that inherits the blob. Largest by the sidecar (tools/engine-size.f on /tmp/hazel-fEE2 with its .names): ROW-WRITE 864, NAME-SPAN, 500, BIND-SOURCE-CALLS 468, IMK-Q+ 388, IMPORT-CHECK 372, IMK-PKG-PUBLICS 364, PROVIDE-TARGET 364. Acceptance: say why the surface cannot reach them - dead private words, or a reaching mechanism the census does not model (name it and teach the census before removing anything) - then either drop the dead spans at build time (the closure strip or native-build, whichever owns the blob) with three generations and test/run.f proving the engine, or record in docs/compiler-measurements.md why each top span must stay. Files: tools/image-size-lib.f, src/habu/aot-closure.f or tools/native-build-core.f, docs/compiler-measurements.md. Verify: engine-size on the new engine reports the surface unreachable-span floor near zero; test/run.f; generation cmp. Depends: none (habu-measure-reachability-across-7032b341 landed). Ownership: engine size. Claim: agent=alder workspace=.jj-ws/alder-tree-shaker-removal.

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

The next reach-model proof adds the capture's explicit `NSTR:IMPORT-ROWS`
name-only root (`tools/native-build-core.f TARGET-IMPORTER`) and follows code
fall-through at the boundaries of indexed bodies and unowned gaps. On the same
3,801,280-byte release engine this removes the false 300-byte dead dictionary
record and changes the surface floor to 2,896 anonymous spans / 58,044 code
bytes; 1,323 fall-through edges are modeled. The terminal decoder treats only
unconditional `B` as ending a body: `BL` is a call and falls through to its
return address. This is a census correction, not a binary reduction. Before/after
dictionary and span candidates have no direct kept-to-dead B/BL edge after the
explicit root is added; capture compaction still needs the offset/relocation
proof and three-generation/full-gate validation. Focused proof:
engine-size-fixtures (both files in one load) and hb-build-test are green on a
private release-engine copy.

The capture compactor now retains named roots, the explicit NSTR importer,
declared code cells, PC-relative targets and fall-through, then removes only
unreachable private bodies. It remaps all code coordinates and relocation
tables, preserving original-window provenance. Bootstrap inline strings,
diagnostics, long names and defer trailers remain data, even when their bytes
decode as instructions. The fixture covers forward/backward branch families,
byte-addressed ADR, code literals, alias entries and malformed provenance.

On the 1ba6e264 tree, the private release host is SHA256
5db92ecab9d53b952b10e3191ee987685dd3db950413701c9c1d81af636bf061,
3,735,744 bytes. Three native-build generations all produce SHA256
3c7570bd6cad2943fc1fd3d48112323d9ff02a4119d23198ceb462de85a618ee,
3,604,672 bytes: 131,072 bytes smaller (3.5%). Native payload code falls from
1,825,324 to 1,758,868 bytes; the shipped dictionary is unchanged (4,871 global,
2,963 package-public and 11 private words, plus 197 package rows). The surface
census reports zero unreachable anonymous spans. This measures engine size,
not a Tender size or runtime-speed improvement.

Validation: 46 selected capture/build registry rows passed (44 in the combined
private run, then the two corrected rows rerun successfully), including the
whole hb-build-fixtures row, whitebox, native positive/negative gates, wide
captures, source-built chain capture and the new compaction fixture. The two
engine-size fixture files also pass together on the final generation, and the
compaction fixture passes on that product. Astra reviewed the implementation
and the corrections with no remaining finding. The full gate and integration
remain with Hazel; this dot stays active until that proof passes.

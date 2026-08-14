---
title: Rename rows row-wise across every seam
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T00:34:27.837902+02:00\""
---

Stage two of habu-rename-over-rows-982167af (whose stage-one refusal names this as the capability it waits for): actually permute multi-cell ADT rows as units through RENAME, which requires the bundle marker to survive every seam the value vector crosses - block arguments at joins, loop edges, CROSS-SCAN, and the return (the ARG-T tables at elaborate.f:1397-1430 each need a parallel row column) - so count and emission agree about runs everywhere, not only inside straight-line code. Acceptance: the four stage-one differentials (swap/rot over a bundle, ADT-returning constructor and user-word calls) compile AND agree with the engine value-for-value; a bundle crossing an if join, a loop edge and a return keeps its integrity (executed differentials); the stage-one refusal is deleted with the capability that replaces it; census delta stated. Files: src/compiler/native/elaborate.f. Depends: habu-rename-over-rows-982167af.

Claim: agent=bundle-seams workspace=.jj-ws/habu-bundle-seams

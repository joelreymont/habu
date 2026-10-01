---
title: Honour owner-private primitive rows in the seal
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:10:08.586559+03:00"
---

Problem: the checker already bounds a package-private PPRIM: row to its package (probe 2026-09-16, aspen: inside admitted, outside refused, reopened admitted), but src/core/internal-mark.f's seal classifies primitives by BARE name at top level and marks the record DNAME-INT, and the engine (src/core/dict.f:252-266) then refuses every checked caller of a private-only row, owner included; outside callers also lose their call window (E-HIR-UNMODELED). So habu-pkg-owned-prim-e08e345f keeps each trust-boundary primitive's global trusted-only row and ADDS an owner-private PPRIM: row that wins inside the owner: two rows for one primitive, a transitional duplication. Acceptance: the seal and the engine's record classification honour an owner-private row (the record carries the owning package, DNAME-INT applies only to rows no package owns), so a primitive may exist as its private row alone; the global trusted-only rows added or kept under habu-pkg-owned-prim-e08e345f are deleted one by one with each owner's callers compiling checked; a fixture proves a private-only primitive callable inside its owner at tier 0 and tier 1 and refused outside by name; PE-TRUSTED-ONLY has no remaining consumer, which is what lets habu-delete-the-trusted-42b30edd delete it. Files: src/core/internal-mark.f, src/core/dict.f, src/core/checker.f (PPRIM;/CLOSE-PRIVATE), src/habu/habu2.f if the seal token changes, bootstrap/cg/forth.fs mirror, test/primitive-trust.f, test/gate-diagnostics-lib.f (F3/F3-FFI pins), test/multi-error-api.f. Verify: the fixture; tools/native-build.f fixpoint; tools/bootstrap.sh; test/run.f. Depends: habu-pkg-owned-prim-e08e345f (aspen). Ownership: seal and record classification. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.

## Brief B11 (2026-10-01 plan): ready now

CLOSE-PRIVATE exists (checker.f ~8191). Worker-max: the seal and record classification honour an owner-private row; test/prim-owner-scope.f's matrix gains the private-only-row case at tier 0 and tier 1, refused outside by name; delete the global trusted-only rows of FFI-PTR>CELL, FFI-CELL>PTR (checker.f ~8782-8788) and addrmap-set. The rows B3/B5 add are deleted by those leaves once this lands. Verify: the fixture; tools/native-build.f byte fixpoint; test/run.f; gforth recovery.

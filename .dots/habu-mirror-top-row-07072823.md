---
title: Mirror top-row.f into the gforth bootstrap prefix list
status: open
priority: 3
issue-type: task
created-at: "2026-07-14T21:31:32.016752+02:00"
---

Found by the toprow lane 2026-07-14: bootstrap/cg/forth.fs carries a parallel copy of the cold-prefix load list that now lacks src/core/top-row.f, so the gforth stage-0 recovery build omits the tier-1 tracker. Not a soundness hole - tools/bootstrap.sh's final native self-refresh rebuilds from habu2.f and restores it - but the stage-0/native prefix lists have drifted, and DDC byte-identity (known gap, dot habu-ddc-cross-check-16562dae) diverges further. Fix: mirror the top-row.f prefix entry into forth.fs (owner-persist lane owns that file - hence this dot rather than a direct edit by the toprow integrator), and consider a lint asserting the two prefix lists stay in sync (the suite-coverage-lint precedent: derive, don't hand-sync). Verify: HABU_ALLOW_BOOTSTRAP gforth recovery produces a tracker-armed stage-0 (or documents the accepted omission), fixpoint x2. Ownership: bootstrap/cg/forth.fs (owner-persist lane).

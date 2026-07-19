---
title: Calibrate whole-gate band for multi-core theft
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T13:40:42.822712+02:00"
---

Follow-up to habu-derive-runtime-budget-81b2f538 (which fixed the runtime slice): the whole-gate attempt-band verdict (test/run-verdict.f; attempt lines print e=<elapsed> b=<calibrated budget>) still false-marginals under partial-core theft, because the calibration spin in lib/test/budget.f is single-threaded - when unrelated user workloads pin two cores (measured 2026-07-19: an Unreal Editor cook plus a zig test loop at 100% CPU each), the spin still finds a free core and reports cal-pct ~115, while the gate's PARALLEL phases lose real throughput and elapsed rises ~5-10% (e=41060 vs b=39200, band=marginal; the same tree scored e=37424 vs b=39780 band=pass when quiet). The marginal path then runs two retry attempts that the harness rules inadmissible on a busy box (empty=f), so the verdict lands marginal-fail with correctness=t. Correct fix shape: make the calibration measure what the gate actually consumes - e.g. a parallel calibration spin sized to the gate's worker fan-out, or derive the band budget from a multi-core-aware signal - with the derivation documented like the runtime-slice fix, the hard verdict preserved, and a negative proof that a genuinely slower engine still fails the band at any load. Do NOT relax the band constant and do NOT make inadmissible retries count as passes.

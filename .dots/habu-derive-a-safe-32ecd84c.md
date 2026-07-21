---
title: Derive a safe nested gate timeout hierarchy
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:14.949753+02:00"
---

candidate validation gives the inner subject and outer worker the same 120-second deadline, leaving no time to capture diagnostics or perform cleanup when the inner limit fires. Define one checked timeout-policy value from measured subject bounds: each enclosing layer must be strictly greater than the maximum inner work plus bounded diagnostic, termination, reap, and cleanup margins. Thread absolute monotonic deadlines rather than independent guessed durations, and report which layer exhausted its allocation. Validate overflow and impossible policies before spawning. Add deterministic fake-clock tests at every boundary, slow diagnostic and cleanup cases, signal/reap paths, host calibration scaling, and proof an inner timeout remains visible before the outer deadline. Preserve nominal healthy budgets and fail closed as uncalibrated where no measured profile exists. Files: candidate validation and gate common/worker timeout policy owners and tests. Verify candidate/gate process slices, host profiles, full cold/hot gate, typed-local/package/host/filemap/dot lints.

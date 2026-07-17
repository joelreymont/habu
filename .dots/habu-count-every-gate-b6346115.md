---
title: Count every gate process boot
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T15:31:41.006752+02:00\""
---

Parent habu-restore-30-second-fcadd9b9. Current helper-spawn telemetry is incomplete: exact traced full run has 183 counted helpers but at least 139 uninstrumented nested validation execs, two proc-pty children, two check-cli raw candidate execs, plus forks/top-level workers. Instrument the shared checked process spawn and fork choke points with gate-installed hooks that record executable path, boot kind, owning label/phase, and candidate/baseline subject without affecting non-gate callers. Replace wrapper-local double counting. Add structural limits for candidate exec boots, total exec boots, and fork count; limits are immutable architecture contracts, not profile baselines. Acceptance: telemetry arithmetic matches retained OS process launches; tests cover raw argv, stdin, async pool, nested suite, and fork paths exactly once; optimized full gate enforces candidate execs <=15, total exec helpers <=25, and reports the first owner crossing the limit.

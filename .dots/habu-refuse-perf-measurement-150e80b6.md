---
title: Refuse perf measurement under machine contention
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:40:47.464642+02:00"
---

Gate-integrity gap proven 2026-07-26 during the wave B1 landing: test/run.f perf attempts carry admissible=t while the machine runs concurrent worker suites, and the verdict then hard-fails on ambient load rather than on the tree - measured e=41354/41410 on the B1 tree and e=43435 on CURRENT MASTER under load, versus e=31451 and e=31013 on the same trees in quiet windows, all five attempts with the identical workload sha 203273826630324995. A hard-fail that tracks contention is a false negative that blocks landings and, worse, a pass under a lucky quiet window on a genuinely regressed tree is a false positive waiting to happen. Behavior: the perf harness samples machine load (loadavg via the existing typed syscall surface, or a calibrated spin-probe if loadavg is unavailable) before and after each attempt and marks the attempt inadmissible - retry, never verdict - when the environment is contended beyond a recorded threshold; the attempt line records the load sample so every verdict is auditable; a maximum-retry exhaustion reports measurement-impossible as its own outcome distinct from performance-fail. Hostile fixture: a synthetic-load leg (spawn a busy child, run one attempt) must yield inadmissible, not hard-fail. Acceptance: attempt lines carry the load fields; the synthetic-load fixture; quiet-window behavior unchanged (band math untouched); test/run.f self-suite green. Owner: the perf phase of the native gate harness (locate the attempt emitter under test/). Dependencies: none. Priority: high - every landing this shift retries on this.

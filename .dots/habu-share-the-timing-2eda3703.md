---
title: Share the timing discipline between harness and workload
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:38:10.247041+02:00"
---

tools/codegen-workload-time.f re-implements the fastest-of-N repetition discipline that tools/codegen-compare-core.f owns - two statements of one methodology, one drift from disagreeing. Factor the timing core (repetitions, fastest-of-N, spread) into one shared file both require; the workload's floor/control-row additions stay its own. Net negative, both tools' printed output byte-identical.

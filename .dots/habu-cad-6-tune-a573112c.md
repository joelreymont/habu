---
title: "CAD 6: TUNE glue - autotuner over schedule objects"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:32:46.922659+02:00"
---

docs/model-cad.md Phases 4/6 integration owner. TUNE drives the habu-ptx-m9-bench autotuner/bench machinery over cad-4 schedule objects: enumerate candidates per schedule family, measure each on device (Orin), write every measurement (not just the winner) into the schedule measurement history, select + cache winner per shape/dtype/layout/target key, report regression vs cached baseline. All candidates appear in the TILE/TUNE structured report; replay by key must reproduce a recorded measurement run. Depends: cad-4-schedule, habu-ptx-m9-bench. Consumed by: cad-7-optimize.

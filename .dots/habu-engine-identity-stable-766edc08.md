---
title: "Engine identity: stable self-path + content key"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T06:42:36.796674+02:00"
---

cad-5 finding: SK-ENGINE$ keeps the engine-unbound placeholder because a --loaded script has no robust way to learn bin/hb's own absolute path (bin/hb resolves only from workspace-root cwd; argv[0] caller-controlled), and a sometimes-real sometimes-placeholder key would fragment the CAD store (forbidden silent fallback). Correct fix: a first-class engine-identity capability - the engine records its own resolved executable path + content key ONCE at startup (engine-side fact, not script-side guess), exposed as a checked word; SK-ENGINE$ then returns the real key and the CAD store keys by engine identity. Also useful to the AOT cache + gate. Files: src/habu (startup path capture), lib exposure, maki/sched-key.f consumer. Rationale recorded in maki/store-replay.f header.

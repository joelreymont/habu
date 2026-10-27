---
title: Give the recovery engine a native product route
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T19:37:27.600869+02:00"
---

docs/bootstrap.md "Recovery routes" says "The recovery engine is a host for a native product build." It is not: the small checked engine tools/bootstrap.sh installs (e.g. at f3d192cd + batch 1) refuses both `hb-build --engine` and `tools/native-build.f` with E-UNDEFINED at NATIVE-RUNTIME:CAPTURE-PREPARE inside APP-IMAGE:SAVE, because only a tools/native-build.f product bakes src/habu/native-runtime.f (src/habu/app-image-core.f SAVE comment, since 2026-09-30). So a machine with no native hb cannot reach a native product from Gforth. Fix the route (the recovery chain must end in a native-build host) or, if a different route exists, state it; the doc sentence must match a run. Proof: from a tree with no bin/hb, tools/bootstrap.sh then `bin/hb --load tools/hb-build.f -- --engine -o <out>` rc 0.

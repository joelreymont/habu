---
title: Cross-build a linux-aarch64 engine from macOS
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.187456+02:00"
---

Problem: native-build --target linux-aarch64 on macOS stops with 'not executable here' (timer lane 1, 2026-10-30), so a Linux engine cannot be produced on the Mac; the roadmap's finish line says a cross-built engine starts. Acceptance: the cross build writes a Linux arm64 engine on macOS and that engine boots on spark. Files: tools/native-build-core.f, tools/native-build.f. Verify: the cross-built engine on spark runs bin/hb --load test/run.f. Depends: none. Ownership: tools/native-build*.f. Claim: unassigned.

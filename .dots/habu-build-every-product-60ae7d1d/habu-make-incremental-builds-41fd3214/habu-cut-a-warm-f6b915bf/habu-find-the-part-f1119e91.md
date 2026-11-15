---
title: Find the part a first warm build recompiles
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.077582+02:00"
---

Problem: the first warm engine build after a cold one in the same out root finds 355 of 356 engine parts fresh; every later warm build finds 356. Seen when the cold build ran through tools/hb-build.f and the warm ones through a measuring driver, so the drivers' different loads may be the cause (src/habu/linker.f REGISTERED? :4128); not established. Acceptance: the part and the cause named; cold then warm through tools/hb-build.f reuses every part, or the recompile is shown required. Depends: none. Claim: unassigned.

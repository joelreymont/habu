---
title: Count listed members, not just started ones
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:01:50.424249+02:00"
---

test/gate-pool.f now proves started-vs-reported (GT-POOL-STARTED/REAPED, fail-loud in GT-POOL-DRAIN), so a launched member that goes dark names itself. What it cannot catch: a deleted GSI-FORK-INCLUDE line — a member that was never launched at all. Detecting that needs the expected membership known outside the file, and a hand-kept manifest is forbidden. Needs a design where the schedule derives from something the build already owns (the suite files on disk, the require graph, or the gate generator) rather than a second list somebody maintains. Do not build a manifest.

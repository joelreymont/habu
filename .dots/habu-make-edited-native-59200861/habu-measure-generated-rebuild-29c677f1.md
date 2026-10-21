---
title: Measure generated rebuild cost
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T13:43:13.271294+03:00"
closed-at: "2026-10-21T13:55:17.214222+03:00"
close-reason: "Measured real unchanged warm path13228hits/11681skips/646compiled with per-process getrusage CPU and separate monotonic wall clocks, no semantic changes.563declaration-generated callbacks consume.892/.737CPU seconds in9.031/7.369CPU-second build brackets (~10%);83others.428/.346s.100TAG/EQ/HASH account for.607/.487s. All generated callbacks were checker-busy; current checking, lowering and publication are included, so after-check machine-body reuse saves less than this upper bound. Clock-only overhead~.42ms total. Exact executable/map stay67665a03/aa2b5d01 and counters unchanged. Instrumentation/cache clone/binaries removed, source empty, worker workspace retired. Separate code-reuse machinery is deferred: modest upper bound, no authority/rollback weakening authorized."
---

Real warm engine rebuilds still compile646definitions; existing diagnosis identifies566checker-busy refusals while generated declarations own an open rollback frame. Their transaction and ordered constructor/accessor authority cannot be bypassed, but their actual CPU cost is unknown. Measure time/counts at the real compile-fallback boundary, distinguishing generated checker-busy definitions from other misses and capture/replay. Use a temporary isolated instrumentation lane, preserve verdicts and exact native executable/map, remove all measurement edits afterward, and report actual CPU versus wall/overhead separately. Do not implement caching, relax idle/authority, change declaration order or create a new reuse seam from counts alone. Result determines whether generated machine-body reuse after current checking deserves implementation.

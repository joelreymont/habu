---
title: "Check the profiler band against the target's DATA size"
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:17:49.801701+02:00"
---

Problem: on the x86-64 harness path (test/x86-64-boot-harness.f -> src/habu/kernel-x64.f -> data-claims.f) the PROF-BAND claim row is computed with the host's DATA-SIZE, not X64LAYOUT:DATA-SIZE, so the target's band is never checked (review 169). Acceptance: DATA-CLAIMS:BAND-ASSERT ( size -- ) restamps the PROF-BAND row and reruns CLAIMS-ASSERT, called with X64LAYOUT:DATA-SIZE in kernel-x64.f beside DP-CEILING; on macOS a cell moved into [$1F7FFC0,$2000000) is refused after the change and silent before. Files: src/habu/data-claims.f, src/habu/kernel-x64.f, the x86-64 harness test. Verify: the overlap probe before/after; the x86-64 harness rows; g1 == g2.

---
title: "Profiler: an inclusive row's caller lines cover only its exclusive samples"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:04:38.295230+02:00"
blocks:
  - habu-profiler-a-small-14b79768
---

Problem: src/habu/prof.f LPROFEDGE (prof.f:93) counts a caller edge only for the word that holds the sampled pc (stated at prof.f:431-437), so under 'by inclusive' a row's caller lines explain only its exclusive samples. In a warm engine build before the inline tag check NFETCH-CHECK:TAGS showed 288 inclusive samples of 1442 and caller lines for 22 of them. The report names no caller for the other 266 and does not say how many words call the row, which is the sign of a call the compiler emits into every word: that tier 0 emitted TAGS before every enum read was found by reading t0.f, not from the profile. Acceptance: the walk counts an edge for every frame it attributes (the frame it finds next above it), so under 'by inclusive' a row's caller lines are shares of all its inclusive samples; each row states how many distinct callers it has; the flat section keeps its exclusive shares; dropped stays 0 for an engine build; docs/debugging.md 'Sampling profiler' says what a caller line counts in each section. Files: src/habu/prof.f, src/habu/prof-abi.f, docs/debugging.md. Verify: bin/hb --load tools/build-profile.f on a warm engine build of a tree at 7eb193e5: the TAGS row under 'by inclusive' gives its caller count and its listed callers' shares add to the listed share of 288, not of 24. Depends: habu-profiler-a-small-14b79768 (an edge per walked frame files the walk's false frames as callers). Coordinate: the edge is counted in the sample handler, which stays machine code; the caller count is report code that habu-write-the-profiler-b7cacfe0 moves into Habu, so it is written there if that dot lands first; src/habu/prof-x64.f takes the same change at x86-64's step. Ownership: src/habu/prof.f, src/habu/prof-abi.f. Claim: unassigned.

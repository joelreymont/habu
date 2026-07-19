---
title: Guard AOT DATA-literal reserve against forged spans
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T21:03:07.028839+02:00"
---

Flagged by the allot-bound lane 2026-07-19: EM-AOT-RELOC-DATA (src/habu/habu2.f:3673) advances DP by a span read from the AOT image with no bound check - the only DP-advancing sink not routed through DP-CHECK. Today it is protected transitively (the span was produced by a same-size engine whose own DP-CHECK bounded it, and images are sha/codesign-verified), so exploitation requires a forged image; but boot-path integrity should not rest on provenance alone. Fix: bound the reserve against DATA-SIZE at load with a fail-closed boot diagnostic (no eval frame exists -> LCOMPILEDIE is inappropriate; follow the existing boot-path die idiom). Red-first: a synthetically oversized span in a test image must die named, a maximal legal span must boot. Engine change -> CODELEN rows same-commit. Territory: src/habu/habu2.f AOT load path + an AOT-negative gate case.

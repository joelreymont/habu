---
title: Make the built-in profiler the profiling tool
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:50:08.039225+03:00"
---

Joel (2026-10-23): profiling stays built in, so any platform Habu runs on can be profiled without external tools; no DWARF work for this. src/habu/prof.f samples per word with callers. Gaps: (1) no per-instruction counts inside a word: add a per-pc histogram for chosen words and print it beside the native disassembly (depends on habu-decode-every-arm64-6753979c); (2) AArch64 only: prof.f refuses any other target's signal frame, so x86-64 needs its frame model; (3) words compiled after prof-on land in the defer column: the index must take definitions published while sampling. (4) the inclusive section undercounts deep frames: dots ls x300 at both tiers showed MAIN 25% and REP 24% inclusive, though every sample runs under them (~/.cache/tmp/jerry-tm-run/prof/dots-ls-t*.out); the stack walk stops early. Acceptance: on ARM64 and x86-64, a workload that defines its own words reports them by name with per-instruction counts for a named word; native-suite rows on both. Claim: unassigned.

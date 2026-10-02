---
title: Refuse every failed reaper fork
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T12:02:57.338938+02:00"
---

Problem (lane 290 r4-held): lib/process-fork.f SPAWN-REAPER (~:198-208) returns RAW's pid unchanged; when the fork fails RAW yields a negative pid and PROC-REAP-ARM-ON's callers receive it as if a reaper were armed, so a capture child runs unwatched and later code may treat the negative value as a pid. Acceptance: a failed reaper fork is refused (named throw or a checked failure the caller handles) at SPAWN-REAPER, and every caller of PROC-REAP-ARM-ON handles it; a test forces the fork failure (RLIMIT_NPROC in a child, or an injected failure through the real path) and shows the refusal. Same class in FORK-REAPER (lib/process-fork.f:157-167, after e68ae79c made its first fork CHECKED): when the intermediate's RAW fork fails it exits 0, and the worker drops its status (`ipid PROC-WAIT-STATUS drop`), so FORK-REAPER returns as if a reaper were armed; the intermediate's exit status must become a throw in the worker. Acceptance also covers: a test that fails only that second fork without racing other processes on a shared host, and the RLIMIT_NPROC case in lib/process-fork-test.f, which cannot refuse root's fork, says so and runs its claim another way (or refuses to run) under root instead of failing its first assertion. Files: lib/process-fork.f and its callers, lib/process-fork-test.f.

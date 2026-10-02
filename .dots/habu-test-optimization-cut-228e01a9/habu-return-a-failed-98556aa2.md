---
title: Return a failed reaper fork as a refusal
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T12:02:57.338938+02:00"
---

Problem (lane 290 r4-held): lib/process-fork.f SPAWN-REAPER (~:198-208) returns RAW's pid unchanged; when the fork fails RAW yields a negative pid and PROC-REAP-ARM-ON's callers receive it as if a reaper were armed, so a capture child runs unwatched and later code may treat the negative value as a pid. Acceptance: a failed reaper fork is refused (named throw or a checked failure the caller handles) at SPAWN-REAPER, and every caller of PROC-REAP-ARM-ON handles it; a test forces the fork failure (RLIMIT_NPROC in a child, or an injected failure through the real path) and shows the refusal. Files: lib/process-fork.f and its callers.

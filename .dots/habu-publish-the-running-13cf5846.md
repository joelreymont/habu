---
title: "Publish the running program's path"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T18:29:47.267386+01:00"
---

Problem: Omabox's installer copies the running binary (Rust current_exe); src/lib/engine-id.f answers it as ENGINE-ID:PATH$ (:129; readlink /proc/self/exe, proc_pidpath on macOS), which Maki and Tender call, but Omabox's request looked for it under PROCESS and found only the private ENGINE-SELF-PATH (:62). Acceptance: PROCESS:OWN-PATH (Joel 2026-11-17) answers the running program's path, throwing E-ENGINE-PATH as CACHED-PATH does; ENGINE-ID calls it instead of its private copy; a built program prints its own path. Depends: Joel's answer on whether ENGINE-ID:PATH$ becomes PROCESS:OWN-PATH. Claim: unassigned.

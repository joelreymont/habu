---
title: Recover checked PTY lifecycle campaign
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T11:33:05.860088+02:00"
---

Forensic sweep 2026-07-19 (dot habu-classify-held-work-5de81ef6): six stranded lanes held the checked PTY lifecycle, owner-PID, and engine trust-ratchet campaign. The linear authority registry lib/process-pty-handle.f plus its focused tests and getpid support landed on master in d093449e/98abafeb; do not recover or overwrite that slice from the old superset. Remaining absent scope is lib/process-pty-io.f, lib/engine-candidate.f, src/os/linux/proc-watch.f, src/os/macos/proc-watch.f, and their integration. Superset lane: habu-pty-integration (24 own commits, recovery snapshot at recover-pty-integration). Subset lanes remain historical inputs until their unlanded pieces are adjudicated. Re-derive each remaining slice against the landed handle API and current engine; do not raw-rebase the old campaign or re-mint already-landed handle ownership.


2026-07-20 recovery scope complete: proc-watch-open, kill-errno, execve (seed, fixpoint-proven), the linear handle registry, the checked supervisor lib/process-pty-io.f, and the shared engine-candidate resolver are all merged and green. The campaign continues with the PTY device layer (child dot).

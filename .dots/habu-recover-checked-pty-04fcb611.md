---
title: Recover checked PTY lifecycle campaign
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T11:33:05.860088+02:00\""
---

Forensic sweep 2026-07-19 (dot habu-classify-held-work-5de81ef6): six stranded lanes hold the checked PTY lifecycle + owner-PID + engine trust ratchet campaign, never merged. Superset lane: habu-pty-integration (24 own commits, working-copy tip preserved as recovery snapshot; bookmark recover-pty-integration, pushed to origin). Its distinctive new files are absent from master: lib/process-pty-handle.f, lib/process-pty-io.f, lib/engine-candidate.f, src/os/linux/proc-watch.f, src/os/macos/proc-watch.f. Subset lanes (retire after harvest): .jj-ws/habu-pty-owner-pid (workspace owner-pid-integration), habu-engine-trust-ratchet, habu-nested-trust-owner, habu-pty-trust-scan, and habu-checker-reject-loader-body (shadow-lint literal + trust-claim work; bookmark recover-loader-body, pushed). The campaign's governing dots do not exist on master and must be re-minted from the recovered content. Recovery path: rebase the superset tip onto current master, re-derive against today's engine (it is ~496 commits behind), review, and land in slices.

Claim: agent=pty-recover workspace=.jj-ws/habu-recover-checked-pty-04fcb611

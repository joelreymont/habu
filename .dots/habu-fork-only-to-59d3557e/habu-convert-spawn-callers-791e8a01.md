---
title: Convert spawn callers to SPAWN requests
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:02.244427+02:00"
blocks:
  - habu-spawn-through-a-6227aeda
---

Fork-to-exec lane 1c: the 96 Habu call sites fill a SPAWN request; then the 17 PROC-SPAWN-* words and four PROC-CWD spawn words are deleted (hard cutover). Client callers (etch 1, maki 5, tender 1, zen 5) are their owners' to convert after it lands; installs never wait. Claim: carl.

phil found two defects that this lane removes, so neither gets its own fix. (1) IO-FORK-TARGET (src/lib/process-pty-io.f ~212): on an empty path the forked child can throw into the parent's copied catch frame (repro /tmp/rev-gate/probe/empty-path.f). The PTY target moves to spawn-fds (design item 4), and spawn validates the path in the parent. Make the repro a case of the PTY conversion. (2) pty-harness.f OPEN-SLAVE opens the slave without close-on-exec and outside FD-LOCK, so a concurrent fork inherits it. Once every child is a spawn that closes the fds past its map (1b C2), the leak cannot happen. Check that OPEN-SLAVE needs no lock after lane 6.

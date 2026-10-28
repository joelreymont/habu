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

---
title: "Spawn a child in the caller's process group"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:23:10.690762+01:00"
---

Problem: process.f SPAWN puts the child in its own group by default and spawn.f's SET-GROUP is private, so Ctrl-C misses a child such as systemctl or journalctl run with the terminal, and a child that reads the terminal from a background group stops (SIGTTIN) while PROCESS:WAIT never returns (bob's /tmp/bob-habu-spawn-group.md and .f). Acceptance: a public request word spawns into the caller's group; bob's reproducer prints equal pgids. Depends: its name, Joel's. Claim: unassigned.

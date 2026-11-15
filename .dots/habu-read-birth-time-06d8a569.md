---
title: Read birth time and subvolume by statx
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T20:22:45.391844+02:00"
---

Problem: Habu has no statx; a program that binds a root to its filesystem identity needs the birth time and the btrfs subvolume (omabox docs/habu-rewrite.md, Habu libraries). Acceptance: FS words answering a descriptor's birth time (and whether the filesystem reports one) and subvolume (and whether reported), Linux; their test. Depends: none. Claim: unassigned.

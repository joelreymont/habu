---
title: "Check include.f's realpath claim"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:41:12.161875+01:00"
closed-at: "2026-11-16T04:07:36.604751+01:00"
---

Problem: src/core/include.f CWD-DIE's comment (:276-279) says glibc answers a too-long realpath whole, but a path resolving to about 5100 bytes through a symlink got false back; the -2 branches of CANON-OS (:256), ROOT-N (:584) and ROOT-SET (:766) look unreached. Acceptance: the comments state what realpath does, shown by a probe, and a branch no input reaches is deleted. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w53rp.

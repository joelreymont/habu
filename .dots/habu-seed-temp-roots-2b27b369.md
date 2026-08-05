---
title: Seed temp roots with the owner, not the moment
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T12:23:19.267184+02:00"
---

lib/fs-mutate.f:244-249 TMPDIR-MKDIR seeds the directory name from mono-ns with no pid — two processes can mint the same root, and a forked child inherits a root it did not make. Flagged independently by two lanes (pool-crash scout and worker). ~20 call sites tree-wide. Fix in the helper once: include the pid (the owner) in the seed; audit call sites whose prose assumes per-process uniqueness.

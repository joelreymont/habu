---
title: Name a working directory the engine cannot hold
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T18:27:36.464539+03:00"
closed-at: "2026-10-15T01:26:45.962631+03:00"
close-reason: Completed or superseded on current master. 37007346 names long cwd failure at include owner; deep-cwd E2E red/green and engine convergence recorded. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

Review 493 (eidlinux), measured on a3a23f35's g1: with a 1252-byte cwd the engine exits 74 with zero bytes on stdout and stderr, for any --load (~/.cache/tmp/kestrel-r4-rev493/deepcwd.{out,err}). A fatal engine exit with no message, the class of 9e94c013. src/lib/process-command.f:234-236 CWD-CHECK already refuses a child cwd over PATH-CAP with E-PROC-PATH, so Habu-spawned children never meet it; a shell can. Acceptance: find the boot step that reads the cwd (getcwd into a PATH-CAP buffer, or the source-root fallback) and make it either work at any cwd the host allows or exit with a one-line named diagnostic ending in LF; a test that starts bin/hb from a cwd longer than PATH-CAP (built by child mkdir/cd, since FS-PATH-CAP bounds MAKE-DIRS) and pins the outcome; rebuild if baked, g1 == g2.

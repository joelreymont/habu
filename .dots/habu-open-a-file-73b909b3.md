---
title: Open a file for reading under an anchor on macOS
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T18:59:41.908538+01:00"
---

Problem: FS:TRY-OPEN-READ-AT ( fd str -- option<fd> ) is defined only under HB-TARGET-LINUX? (src/lib/fs-anchored.f:309-437), so Maki's import_solidworks, which checks a received path names a regular file reached with no symbolic link without reading it, does not build for macOS (tim's request, /tmp/tim-habu-open-read-at.md). Acceptance: on macOS FS:TRY-OPEN-READ-AT has Linux's stack effect and none/throw contract, FS:ERRNO set after none, and a program calling it builds and runs there. Depends: macOS work resumes (Joel: no macOS work for now). Claim: unassigned.

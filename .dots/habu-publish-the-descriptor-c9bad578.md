---
title: Publish the descriptor words a sync needs
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T20:22:45.382031+02:00"
---

Problem: Habu cannot fstat a descriptor publicly, read its inode or device, ask a directory's name length limit, chmod or seek a descriptor, list a directory by descriptor, or name most errno values (omabox docs/habu-rewrite.md, Habu libraries). Acceptance: those words in fs.f, fd.f and fs-list.f, with the errno constants NOENT EXIST LOOP NODATA XDEV RANGE NOTDIR ACCES PERM NOTEMPTY ISDIR STALE MFILE NFILE NOMEM NOSPC public in FS; their tests. Depends: none. Claim: unassigned.

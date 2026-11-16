---
title: Throw on a failed write from type
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:23:10.650191+01:00"
---

Problem: type drops a failed write: 20000 lines through type cr under FD:WITH-NOSIGPIPE, piped into head -c 10 (EPIPE) or to /dev/full (ENOSPC), end with code 0 and rc 0 (bob's /tmp/bob-habu-type-write.md and .f); forth-card.md says nothing, so a CLI exits 0 after losing its output. Acceptance: a write type, emit or cr cannot complete throws the failure as FD:WRITE-FULL does; both reproducer runs end nonzero with the record; the card states it. Depends: none. Claim: unassigned.

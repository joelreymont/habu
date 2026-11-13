---
title: "Move PROC-NONBLOCK! and tcp4's O_NONBLOCK onto FD:NONBLOCK!"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:08:45.256521+02:00"
---

Problem: src/lib/process.f PROC-NONBLOCK! (:565) repeats FD's FLAGS/ARM without the kept-flag check FD:NONBLOCK! has (ba1b7627, XNU stores the flags before a kqueue refuses FIONBIO); src/lib/net/test-issuer-test.f:522 calls it outside a capture row; src/lib/net/tcp4.f O-NONBLOCK (:132) writes O_NONBLOCK's number a third time. Joel 2026-11-05: one public word in FD. Found by the signal review. Acceptance: PROC-NONBLOCK! and O-NONBLOCK gone, their callers use FD:NONBLOCK!; O_NONBLOCK's number written only in fd.f. Files: src/lib/process.f, src/lib/net/tcp4.f, src/lib/net/test-issuer-test.f. Verify: process-test, tcp4/http tests touching nonblocking accept, test-issuer-test on macOS and spark. Depends: none. Ownership: unassigned. Claim: unassigned.

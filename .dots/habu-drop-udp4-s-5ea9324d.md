---
title: "Drop UDP4's checks of what the kernel answers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:16:49.424618+01:00"
---

Problem: src/lib/net/udp4.f checks at run time what the kernel or a type already says: CHECK-SOCKET (:88, the kernel's EBADF), SEND's size range (:310, EMSGSIZE), RECEIVE's capacity range (:325, harmless), ENDPOINT@'s length and family checks (:130-136, kernel-guaranteed); and its E-PLATFORM branches (:116-119, :132-134), UDP4:E-PLATFORM and the header's 'Another OS is E-PLATFORM' (:2) cannot run under src/lib/ffi-abi.f:30's ?TARGET (UDP design, 2026-11-17). Acceptance: those checks and branches are gone; a bad socket, size or capacity answers the kernel's failed errno; the udp4 suite passes. Depends: none. Claim: unassigned.

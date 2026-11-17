---
title: "Put TCP4's socket and deadline logic in one place"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:16:49.444646+01:00"
---

Problem: src/lib/net/tcp4.f carries its own copies of socket open and flags (:120, :317, :325), errno (:301), the $7FFFFFFF timeout bound (:138, :407) and remaining-time arithmetic (:509), which the datagram layer (src/lib/net/datagram.f) holds for datagrams (UDP design, 2026-11-17). Acceptance: TCP4 and DATAGRAM share one copy of what both do (open, errno, deadline), no $7FFFFFFF bound remains, the tcp4 and udp4 suites pass. Depends: none. Claim: unassigned.

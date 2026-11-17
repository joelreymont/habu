---
title: "Measure DATAGRAM's macOS receive buffer"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:46:41.190417+01:00"
---

Problem: src/lib/net/datagram.f TRY (:99-115) allocates SO_RCVBUF bytes per receive try on macOS so it can see a datagram's full length; UDP4 used 65507. macOS's default net.inet.udp.recvspace may be ~787 KB (unmeasured; the UDP review's L12). Acceptance: sysctl net.inet.udp.recvspace and a UDP4:RECEIVE try's allocation measured on macOS; if the cost matters, the buffer is bounded by the family's largest datagram. Depends: none. Claim: unassigned.

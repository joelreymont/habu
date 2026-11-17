---
title: "Drop TCP4's and SERIAL's runtime checks"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T09:26:19.050575+01:00"
---

Problem: src/lib/net/tcp4.f and src/lib/serial.f keep the checks UDP4 dropped (habu-drop-udp4-s-5ea9324d): E-PLATFORM arms that src/lib/ffi-abi.f's ?TARGET makes unreachable (tcp4.f:123-172, :248; serial.f:50-62, :169, :180, :219), TCP4's CHECK-FD (:208, the kernel's EBADF), and SERIAL's E-RESULT checks of what the kernel answered (:236, :242). Acceptance: those checks and arms are gone, each failure the kernel reports answers its errno, and the tcp4 and serial tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-tcpchk.

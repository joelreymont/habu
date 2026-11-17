---
title: AIO takes a descriptor as the 32-bit int the kernel reads
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T09:54:00.933774+01:00"
---

Problem: AIO:POLL and AIO:READ (src/lib/aio.f:1160, :1251) hand FD>N's whole cell to the host stage. Linux stores its low 32 bits into the SQE (LE:U32!), but Darwin's MAC-CLAIM -> MAC-FD-ROOM -> FD-GROW (src/lib/aio-macos.f:106-124) sizes its row table as (fd+1)*24 bytes with nothing bounding fd: fd $4000000000000000 wraps the size to 24 bytes while FD-CAP becomes 2^62+1, so the old rows' copy and later rows write past the mapping. Since TCP4 and SERIAL dropped CHECK-FD/CHECK-HANDLE (dot 71de9ef4), TCP4:>CONNECTION and SERIAL:>HANDLE reach it with any cell. Acceptance: every AIO submission takes the descriptor as the sign-extended low 32 bits, as the kernel reads it, so a negative one keeps Darwin's EBADF route and no fd past 2^31 reaches the table; no runtime check added. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-aiofd.

---
title: "Take str in TCP4's send words"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T16:41:08.155333+01:00"
closed-at: "2026-11-23T10:11:21.730490+01:00"
---

Problem: TCP4 SEND-SLICE, TRY-SEND and WRITE (src/lib/net/tcp4.f:621,637,648) read bytes as ^u8 NUM:byte-len, so about 50 callers convert with TCP4:TRANSFER-BYTES. Acceptance: the three say str, callers drop the conversion, TRANSFER-BYTES goes if unused, the suites loading them pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2tcp

---
title: Give T0-arm64 calls and code addresses
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T17:15:13.707824+02:00"
closed-at: "2026-11-02T11:22:38.479846+02:00"
close-reason: "Landed d86ef441ef 'Give T0-arm64 calls and code addresses'."
---

L5b1, worker in .jj-ws.noindex/jerry-t0-l1 as a new commit on txplslym. T0-A64 CALL TAIL CALL-SELF ADDR-CODE ADDR-QUOT: bl/b rows filed at CLOSE and written by PATCH, LINKED x30 frame iff CALL or CALL-SELF, TAIL leaves through the epilogue, CALL-SELF resolved at CLOSE with no row, ADDR-CODE a four-lane chain with an ADDR-CODE row, ADDR-QUOT a PC-relative adr QUOT-OPEN writes, with no row or patch. Proof: .out/t0-dis-a64.f and .out/t0-dis.f all PASS, x86 bins unchanged against .out/dis-x64-l2b.

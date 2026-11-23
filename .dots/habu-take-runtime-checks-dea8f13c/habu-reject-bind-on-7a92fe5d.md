---
title: Reject BIND on a view WITH-MUT did not lend
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T10:25:50.391214+01:00"
closed-at: "2026-11-23T18:48:58.160940+01:00"
---

Problem: the checker accepts C2-MEM:BIND inside a WITH-INIT whose view came from C2-MEM:ALLOC (src/core/check-walk.f:57-62 takes any mut-view<..,u8> parent), so ROOT-FROM (src/lib/c2-memory.f:194) finds no frame and throws E-C2-STATE at run time; reproducer /tmp/jerry-p1/c2s-probe/probe.f. Acceptance: the checker rejects that BIND with a rejection that names the fix; ROOT-FROM's throw is deleted; the c2 suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p8bind.

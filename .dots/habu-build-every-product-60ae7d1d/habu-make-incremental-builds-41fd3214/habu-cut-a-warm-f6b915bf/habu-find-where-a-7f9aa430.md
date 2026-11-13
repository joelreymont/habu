---
title: "Find where a warm engine build's 2 s goes"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:24:00.708550+02:00"
closed-at: "2026-11-13T04:37:15.294582+02:00"
---

Problem: a warm engine build that compiles nothing takes 2.03 s wall (2026-11-13, installed engine at master 1c05eda7); the kernel build accounts for about 0.5 s (parent dot), the rest is not attributed. tools/build-profile.f's flat profile names DYNAMIC-STORAGE:ADDRESS (10% self), the store guard (PROT-SPAN) 8%, LE:U64@ 14% inclusive and BUF:SPAN$'s live check 20% inclusive, through LINKER:BYTES$ per object field (src/habu/linker.f:513, RD-U64 :2739). Acceptance: the wall time split into phases that add up to it, each cost's cause as path:line, and the cuts ranked by the time each saves, as a report the fix briefs cite. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-warm.

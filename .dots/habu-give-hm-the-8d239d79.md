---
title: Give HM the capacity n keys need
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T14:30:07.487226+01:00"
---

Problem: a caller of HM:PROBE must pick a capacity that is a nonzero power of two (CAP-OK, src/lib/hashmap.f:12-16) with room to spare, and Maki computes it twice by hand: kernel.f ROOM-FOR (16 begin dup k 2 * < while 2 * repeat) and ops.f KIND-ROOM (the least power of two at least twice n). Choosing a capacity HM accepts is HM's rule, so it belongs beside CAP-OK (found by the Maki review of impkind, 2026-11-17). Acceptance: one public HM word answering the least valid capacity for n keys at HM's load factor, its contract in its comment; Maki's two copies then call it. Depends: none. Claim: unassigned.

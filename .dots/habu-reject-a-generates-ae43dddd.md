---
title: "Reject a generates: row on an opener that leaves its definition open"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:07:38.628677+01:00"
---

Problem: a definer with a generates: row whose body leaves a definition open (an opener) is accepted, and the definition's ';' is then judged by nothing: /tmp/jerry-w204-rr/p/g1.f (such a definer, then 'OPENW Q drop ;') is load 70, check 70, --verify-only 0 with nothing reported. Acceptance: --verify-only rejects or warns on g1.f as the load does. Depends: none. Claim: unassigned.

---
title: Recover throws across evaluate in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.957323+03:00"
---

Problem: throw recovery across `evaluate` is assembly (src/habu/habu2.f LEVCORRUPT, :2963, validates the handler frame). It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix it in the code that stays, never in the machinery the parent replaces.
Acceptance: rc propagation across `evaluate` and handler-frame validation, fail-closed as `LEVCORRUPT`, in src/habu/interpreter.f, with throw-across-`evaluate` cases through the Habu loop, including a corrupted frame.
Depends: none.
Claim: unassigned.

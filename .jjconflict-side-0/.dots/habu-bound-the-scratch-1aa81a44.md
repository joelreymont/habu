---
title: Bound the scratch a failed builder eats
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T18:21:25.671734+02:00"
---

A failed NEW-BUILDER permanently consumes context scratch (bump allocator, no free) even though its arena slots now release — a context that retries builder creation many times hits E-IR-CTX-SCRATCH instead of E-IR-ARENA-SLOTS, which is Storage.v FINDING 3's arithmetic reached by a new route. Stated in the source, not papered over. Decide: either scratch scoping (release with the arena scope) or a documented retry bound with the refusal named. Found by the CG-07 lane 2026-08-05.
